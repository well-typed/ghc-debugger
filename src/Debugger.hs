{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications #-}
module Debugger where

import System.Exit
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits (xor)

import GHC
import GHC.Builtin.Names (gHC_INTERNAL_GHCI_HELPERS)
import GHC.Data.FastString
import GHC.Data.Maybe (expectJust)
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env as GHC
import GHC.Driver.Monad
import GHC.Driver.Ppr as GHC
import GHC.Runtime.Debugger.Breakpoints
import GHC.Runtime.Eval.Types as GHC
import GHC.Runtime.Eval
import GHC.Types.Breakpoint
import GHC.Types.Id
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader as RdrName (mkOrig, globalRdrEnvElts, greName)
import GHC.Types.SrcLoc
import GHC.Unit.Module.Env as GHC
import GHC.Utils.Outputable as GHC
import qualified GHC.Runtime.Debugger as GHCD
import qualified GHC.Runtime.Heap.Inspect as GHCI
import qualified GHCi.Message as GHCi
import qualified GHC.Unit.Home.Graph as HUG

import Data.Maybe
import Control.Monad.Reader
import Data.IORef

import Debugger.Monad
import Debugger.Interface.Messages

--------------------------------------------------------------------------------
-- * Executing commands
--------------------------------------------------------------------------------

-- | Execute the given debugger command in the current 'Debugger' session
execute :: Command -> Debugger Response
execute = \case
  ClearFunctionBreakpoints -> DidClearBreakpoints <$ clearBreakpoints Nothing
  ClearModBreakpoints fp -> DidClearBreakpoints <$ clearBreakpoints (Just fp)
  SetBreakpoint bp -> DidSetBreakpoint <$> setBreakpoint bp BreakpointEnabled
  DelBreakpoint bp -> DidRemoveBreakpoint <$> setBreakpoint bp BreakpointDisabled
  GetStacktrace -> GotStacktrace <$> getStacktrace
  GetScopes -> GotScopes <$> getScopes
  GetVariables kind -> GotVariables <$> getVariables kind
  DoEval exp_s -> DidEval <$> doEval exp_s
  DoContinue -> DidContinue <$> doContinue
  DoSingleStep -> DidStep <$> doSingleStep
  DoStepLocal -> DidStep <$> doLocalStep
  DebugExecution { entryPoint, runArgs } -> DidExec <$> debugExecution entryPoint runArgs
  TerminateProcess -> liftIO $ do
    -- Terminate!
    putStrLn "Goodbye..."
    exitWith ExitSuccess

--------------------------------------------------------------------------------
-- * Breakpoints
--------------------------------------------------------------------------------

-- | Remove all module breakpoints set on the given loaded module by path
--
-- If the argument is @Nothing@, clear all function breakpoints instead.
clearBreakpoints :: Maybe FilePath -> Debugger ()
clearBreakpoints mfile = do
  -- It would be simpler to go to all loaded modules and disable all
  -- breakpoints for that module rather than keeping track,
  -- but much less efficient at scale.
  hsc_env <- getSession
  bids <- getActiveBreakpoints mfile
  forM_ bids $ \bid -> do
    GHC.setupBreakpoint hsc_env bid (breakpointStatusInt BreakpointDisabled)

  -- Clear out the state
  bpsRef <- asks activeBreakpoints
  liftIO $ writeIORef bpsRef emptyModuleEnv

-- | Set a breakpoint in this session
setBreakpoint :: Breakpoint -> BreakpointStatus -> Debugger BreakFound
setBreakpoint ModuleBreak{path, lineNum, columnNum} bp_status = do
  mod <- getModuleByPath path

  mticks <- makeModuleLineMap (ms_mod mod)
  let mbid = do
        ticks <- mticks
        case columnNum of
          Nothing -> findBreakByLine lineNum ticks
          Just col -> findBreakByCoord (lineNum, col) ticks

  case mbid of
    Nothing -> do
      liftIO $ putStrLn "todo: Reply saying breakpoint was not set because the line doesn't exist."
      return $ BreakFoundNoLoc False
    Just (bix, span) -> do
      let bid = BreakpointId { bi_tick_mod = ms_mod mod
                             , bi_tick_index = bix }
      changed <- registerBreakpoint bid bp_status ModuleBreakpointKind
      return $ BreakFound
        { changed = changed
        , sourceSpan = realSrcSpanToSourceSpan span
        , breakId = bid
        }
setBreakpoint FunctionBreak{function} bp_status = do
  resolveFunctionBreakpoint function >>= \case
    Left e -> error (showPprUnsafe e)
    Right (mod, mod_info, fun_str) -> do
      let modBreaks = GHC.modInfoModBreaks mod_info
      case findBreakForBind fun_str modBreaks of
        [] -> error ("No breakpoint found by name " ++ function)
        [(bix, span)] -> do
          let bid = BreakpointId { bi_tick_mod = mod
                                 , bi_tick_index = bix }
          changed <- registerBreakpoint bid bp_status FunctionBreakpointKind
          return $ BreakFound
            { changed = changed
            , sourceSpan = realSrcSpanToSourceSpan span
            , breakId = bid
            }
        xs -> error ("Ambiguous breakpoint found by name " ++ function ++ ": " ++ show xs)
setBreakpoint exception_bp bp_status = do
  let ch_opt | BreakpointDisabled <- bp_status
             = gopt_unset
             | otherwise
             = gopt_set
      opt | OnUncaughtExceptionsBreak <- exception_bp
          = Opt_BreakOnError
          | OnExceptionsBreak <- exception_bp
          = Opt_BreakOnException
  dflags <- GHC.getInteractiveDynFlags
  let
    -- changed if option is ON and bp is OFF (breakpoint disabled), or if
    -- option is OFF and bp is ON (i.e. XOR)
    breakOn = bp_status /= BreakpointDisabled
    didChange = gopt opt dflags `xor` breakOn
  GHC.setInteractiveDynFlags $ dflags `ch_opt` opt
  return (BreakFoundNoLoc didChange)

--------------------------------------------------------------------------------
-- * Evaluation
--------------------------------------------------------------------------------

-- | Run a program with debugging enabled
debugExecution :: EntryPoint -> [String] {-^ Args -} -> Debugger EvalResult
debugExecution entry args = do

  -- consider always using :trace like ghci-dap to always have a stacktrace?
  -- better solution could involve profiling stack traces or from IPE info?

  (entryExp, exOpts) <- case entry of

    MainEntry nm -> do
      let prog = fromMaybe "main" nm
      wrapper <- mkEvalWrapper prog args -- bit weird that the prog name is the expression but fine
      let execWrap' fhv = GHCi.EvalApp (GHCi.EvalThis wrapper) (GHCi.EvalThis fhv)
          opts = GHC.execOptions {execWrap = execWrap'}
      return (prog, opts)

    FunctionEntry fn ->
      -- TODO: if "args" is unescaped (e.g. "some", "thing"), then "some" and
      -- "thing" will be interpreted as variables. To pass strings it needs to
      -- be "\"some\"" "\"things\"".
      return (fn ++ " " ++ unwords args, GHC.execOptions)

  GHC.execStmt entryExp exOpts >>= handleExecResult

  where
    -- It's not ideal to duplicate these two functions from ghci, but its unclear where they would better live. Perhaps next to compileParsedExprRemote? The issue is run
    mkEvalWrapper :: GhcMonad m => String -> [String] ->  m ForeignHValue
    mkEvalWrapper progname' args' =
      runInternal $ GHC.compileParsedExprRemote
      $ evalWrapper' `GHC.mkHsApp` nlHsString progname'
                     `GHC.mkHsApp` nlList (map nlHsString args')
      where
        nlHsString = nlHsLit . mkHsString
        evalWrapper' =
          GHC.nlHsVar $ RdrName.mkOrig gHC_INTERNAL_GHCI_HELPERS (mkVarOccFS (fsLit "evalWrapper"))

    -- run internal here serves to overwrite certain flags while executing the
    -- internal "evalWrapper" computation which is not relevant to the user.
    runInternal :: GhcMonad m => m a -> m a
    runInternal =
        withTempSession mkTempSession
      where
        mkTempSession = hscUpdateFlags (\dflags -> dflags
          { -- Disable dumping of any data during evaluation of GHCi's internal expressions. (#17500)
            dumpFlags = mempty
          }
              -- We depend on -fimplicit-import-qualified to compile expr
              -- with fully qualified names without imports (gHC_INTERNAL_GHCI_HELPERS above).
              `gopt_set` Opt_ImplicitImportQualified
          )


-- | Resume execution of the stopped debuggee program
doContinue :: Debugger EvalResult
doContinue = GHC.resumeExec RunToCompletion Nothing >>= handleExecResult

-- | Resume execution but only take a single step.
doSingleStep :: Debugger EvalResult
doSingleStep = GHC.resumeExec SingleStep Nothing >>= handleExecResult

-- | Resume execution but stop at the next tick within the same function.
doLocalStep :: Debugger EvalResult
doLocalStep = do
  GHC.getResumeContext >>= \case
    [] -> error "doing local step but not stopped at a breakpoint?!" 
    r:_ -> do
      GHC.resumeExec (LocalStep (resumeSpan r)) Nothing >>= handleExecResult

-- | Evaluate expression. Includes context of breakpoint if stopped at one (the current interactive context).
doEval :: String -> Debugger EvalResult
doEval exp = do
  excr <- GHC.execStmt exp GHC.execOptions
  case excr of
    ExecBreak{} -> continueToCompletion >>= handleExecResult
    ExecComplete{} -> handleExecResult excr

-- | Turn a GHC's 'ExecResult' into an 'EvalResult' response
handleExecResult :: GHC.ExecResult -> Debugger EvalResult
handleExecResult = \case
    ExecComplete {execResult} -> do
      case execResult of
        Left e -> return (EvalException (show e) "SomeException")
        Right [] -> error $ "Nothing bound for expression"
        Right (n:ns) -> inspectName n >>= \case
          Just VarInfo{varValue, varType} -> return (EvalCompleted varValue varType)
          Nothing     -> liftIO $ fail "doEval failed"
    ExecBreak {breakNames, breakPointId=Nothing} ->
      -- Stopped at an exception
      -- todo: force exception to display string of exception?
      return EvalStopped{breakId = Nothing}
    ExecBreak {breakNames, breakPointId} ->
      return EvalStopped{breakId = toBreakpointId <$> breakPointId}

-- | Resume execution with single step mode 'RunToCompletion', skipping all breakpoints we hit, until we reach 'ExecComplete'.
--
-- We use this in 'doEval' because we want to ignore breakpoints in expressions given at the prompt.
continueToCompletion :: Debugger ExecResult
continueToCompletion = do
  execr <- GHC.resumeExec RunToCompletion Nothing
  case execr of
    ExecBreak{} -> continueToCompletion
    ExecComplete{} -> return execr

--------------------------------------------------------------------------------
-- * Stack trace
--------------------------------------------------------------------------------

-- | Get the stack frames at the point we're stopped at
getStacktrace :: Debugger [StackFrame]
getStacktrace = do
  topStackFrame <- GHC.getResumeContext >>= \case
    [] -> error "not stopped at a breakpoint?!"
    r:_ -> do
      case (srcSpanToRealSrcSpan $ GHC.resumeSpan r) of
        Just ss -> do
          return $ Just StackFrame {
            name = GHC.resumeDecl r
          , sourceSpan = realSrcSpanToSourceSpan ss
          }
        Nothing ->
          -- No resume span; which should mean we're stopped on an exception
          return Nothing

  return $ catMaybes [topStackFrame]

--------------------------------------------------------------------------------
-- * Scopes
--------------------------------------------------------------------------------

-- | Get the stack frames at the point we're stopped at
getScopes :: Debugger [ScopeInfo]
getScopes = do
  GHC.getResumeContext >>= \case
    [] -> error "not stopped at a breakpoint?!"
    r:_ -> case (srcSpanToRealSrcSpan $ GHC.resumeSpan r) of
      Just rss -> do
        let sourceSpan = realSrcSpanToSourceSpan rss
        return
          [ ScopeInfo { kind = LocalVariablesScope
                      , expensive = False
                      , numVars = Nothing
                      , sourceSpan
                      }
          , ScopeInfo { kind = ModuleVariablesScope
                      , expensive = False
                      , numVars = Nothing
                      , sourceSpan
                      }
          , ScopeInfo { kind = GlobalVariablesScope
                      , expensive = False
                      , numVars = Nothing
                      , sourceSpan
                      }
          ]
      Nothing ->
        -- No resume span; which should mean we're stopped on an exception
        -- TODO: Use exception context to create source span, or at least
        -- return the source span null to have Scopes at least.
        return []

--------------------------------------------------------------------------------
-- * Variables
--------------------------------------------------------------------------------

-- | Get variables using a variable/variables reference
getVariables :: VariableReference -> Debugger [VarInfo]
getVariables vk = do
  hsc_env <- getSession
  GHC.getResumeContext >>= \case
    [] -> error "not stopped at a breakpoint?!"
    r:_ -> case vk of
      SpecificVariable n -> do
        -- Only force thing when scrutinizing specific variable
        -- TODO: Would it be possible to unwrap one layer without expanding children?
        -- TODO: Try setting depth=1
        lookupVarByReference n >>= \case
          Nothing -> error "lookupVarByReference failed"
          Just i -> do
            term <- GHC.obtainTermFromId 100{-depth == 1 only-} True{- force! -} i
            let isThunk
                 | Suspension{} <- term = True
                 | otherwise = False
            varName <- display i
            varType <- display (GHCI.termType term)
            varValue <- display =<< GHCD.showTerm term
            -- varRef indicates how to fetch fields of this variable,
            -- but this request is already responding to that!
            -- the VarInfo for this @SpecificVariable@ must have @NoVariables@ reference.
            -- Other fields may have others.
            let varRef = NoVariables
            return [VarInfo{..}]


      LocalVariables ->
        -- bindLocalsAtBreakpoint hsc_env (GHC.resumeApStack r) (GHC.resumeSpan r) (GHC.resumeBreakpointId r)
        mapM tyThingToVarInfo =<< GHC.getBindings

      ModuleVariables -> do
        things <- filter (sameMod r) <$> hugGlobalVars hsc_env r
        mapM (\(n, tt) -> do
          name <- display n
          vi <- tyThingToVarInfo tt
          return vi{varName = name}) things

      GlobalVariables -> do
        things <- filter (not . sameMod r) <$> hugGlobalVars hsc_env r
        mapM (\(n, tt) -> do
          name <- display n
          vi <- tyThingToVarInfo tt
          return vi{varName = name}) things

      NoVariables -> do
        return []
  where
    sameMod r (nameModule -> modl, _) = (ibi_tick_mod <$> GHC.resumeBreakpointId r) == Just modl
    hugGlobalVars hsc_env r = catMaybes <$> mapM (select hsc_env) (globalElts r)
    globalElts = globalRdrEnvElts . igre_env . snd . GHC.resumeBindings
    select hsc_env (greName -> n) = liftIO $ do
      let modl = nameModule n
      hmi <- HUG.lookupHugByModule modl (hsc_HUG hsc_env)
      case hmi of
        Nothing ->
          -- not in home units, don't show.
          pure Nothing
        Just _ -> do
          mty <- GHC.lookupType hsc_env n
          case mty of
            Nothing ->
              pure Nothing
            Just ty ->
              pure $ Just (n, ty)

--------------------------------------------------------------------------------
-- * GHC Utilities
--------------------------------------------------------------------------------

-- | Get the value and type of a given 'Name' as rendered strings in 'VarInfo'.
inspectName :: Name -> Debugger (Maybe VarInfo)
inspectName n = do
  GHC.lookupName n >>= \case
    Nothing -> do
      liftIO . putStrLn =<< display (text "Failed to lookup name: " <+> ppr n)
      pure Nothing
    Just tt -> Just <$> tyThingToVarInfo tt

-- | 'TyThing' to 'VarInfo'. The 'Bool' argument indicates whether to force the
-- value of the thing (as in @True = :force@, @False = :print@)
tyThingToVarInfo :: TyThing -> Debugger VarInfo
tyThingToVarInfo = \case
  t@(AConLike c) -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  t@(ATyCon c)   -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  t@(ACoAxiom c) -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  AnId i -> do
    term <- GHC.obtainTermFromId 100{-depth-} False{- force only on request, in other calls to obtainTermFromId -} i
    let isThunk
         | Suspension{} <- term = True
         | otherwise = False
    varName <- display i
    varType <- display (GHCI.termType term)
    varValue <- display =<< GHCD.showTerm term

    -- The VarReference allows user to expand variable structure and inspect value.
    -- Here, we do not want to allow "expanding" a value that is fully computed.
    -- We only want to return @SpecificVariable@ (which allows expansion) for
    -- values with sub-fields or thunks. Computed values should return @NoVariables@.
    varRef <-
      if not isThunk
       then return NoVariables
       else lookupVarById i >>= \case
        Nothing -> do
          ir <- freshInt
          insertVarBimap ir i
          return (SpecificVariable ir)
        Just ir ->
          return (SpecificVariable ir)

    return VarInfo{..}

-- | Convert a GHC's src span into an interface one
realSrcSpanToSourceSpan :: RealSrcSpan -> SourceSpan
realSrcSpanToSourceSpan ss = SourceSpan
  { file = unpackFS $ srcSpanFile ss
  , startLine = srcSpanStartLine ss
  , startCol = srcSpanStartCol ss
  , endLine = srcSpanEndLine ss
  , endCol = srcSpanEndCol ss
  }

--------------------------------------------------------------------------------
-- * General utilities
--------------------------------------------------------------------------------

-- | Display an Outputable value as a String
display :: Outputable a => a -> Debugger String
display x = do
  dflags <- getDynFlags
  return $ showSDoc dflags (ppr x)
{-# INLINE display #-}

