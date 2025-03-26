{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, NamedFieldPuns, TupleSections, LambdaCase #-}
module Debugger.Monad where

import Prelude hiding (mod)
import Data.Function
import System.Exit
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (assert)

import Control.Monad.Catch

import qualified GHC
import qualified GHCi.BreakArray as BA
import GHC (Name)
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Phases as GHC
import GHC.Driver.Pipeline as GHC
import GHC.Driver.Config.Logger as GHC
import GHC.Driver.Session.Units as GHC
import GHC.Unit.Module.ModSummary as GHC
import GHC.Driver.Session.Mode as GHC
import GHC.Driver.Session.Lint as GHC
import GHC.Utils.Outputable as GHC
import GHC.Utils.Monad as GHC
import GHC.Utils.Logger as GHC
import GHC.Types.Unique.Supply as GHC
import GHC.Runtime.Loader as GHC
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Heap.Inspect
import GHC.Unit.Module.Env as GHC
import GHC.Driver.Env

import Data.IORef
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List
import qualified Data.IntMap as IM

import Control.Monad.Reader

import Debugger.Interface.Messages

-- | A debugger action.
newtype Debugger a = Debugger { unDebugger :: ReaderT DebuggerState GHC.Ghc a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , GHC.HasDynFlags, MonadReader DebuggerState )

-- | State required to run the debugger.
--
-- - Keep track of active breakpoints to easily unset them all.
data DebuggerState = DebuggerState
      { activeBreakpoints :: IORef (ModuleEnv (IM.IntMap (BreakpointStatus, BreakpointKind)))
        -- ^ Maps a 'BreakpointId' in Trie representation to the
        -- 'BreakpointStatus' it was activated with.
      , varReferences     :: IORef (IM.IntMap (Name, Term))
      -- ^ When we're stopped at a breakpoint, this maps variable reference to
      -- Terms to allow further inspection and forcing by reference.
      --
      -- This map is only valid while stopped in this context. After stepping
      -- or resuming evaluation in any available way, this map becomes invalid
      -- and should therefore be cleaned.
      , genUniq           :: IORef Int
      -- ^ Generates unique ints
      }

-- TODO: The varReferences map should probably live in DebugAdaptorState and be stored by them?

-- | Enabling/Disabling a breakpoint
data BreakpointStatus
      -- | Breakpoint is disabled
      --
      -- Note: this must be the first constructor s.t.
      --  @BreakpointDisabled < {BreakpointEnabled, BreakpointAfterCount}@
      = BreakpointDisabled
      -- | Breakpoint is enabled
      | BreakpointEnabled
      -- | Breakpoint is disabled the first N times and enabled afterwards
      | BreakpointAfterCount Int
      deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Run a 'Debugger' action on a session constructed from a given GHC invocation.
runDebugger :: FilePath -- ^ The libdir (given with -B as an arg)
            -> [String]       -- ^ The list of units included in the invocation
            -> [String]       -- ^ The full ghc invocation (as constructed by hie-bios flags)
            -> Debugger a     -- ^ 'Debugger' action to run on the session constructed from this invocation
            -> IO a
runDebugger libdir units ghcInvocation' (Debugger action) = do
  let ghcInvocation = filter (\case ('-':'B':_) -> False; _ -> True) ghcInvocation'

  GHC.runGhc (Just libdir) $ do

    dflags0 <- GHC.getSessionDynFlags

    let dflags1 = dflags0
          { GHC.ghcMode = GHC.CompManager
          , GHC.backend = GHC.interpreterBackend
          , GHC.ghcLink = GHC.LinkInMemory
          , GHC.verbosity = 1
          }
          -- Default GHCi settings
          `GHC.gopt_set` GHC.Opt_ImplicitImportQualified
          `GHC.gopt_set` GHC.Opt_IgnoreOptimChanges
          `GHC.gopt_set` GHC.Opt_IgnoreHpcChanges
          `GHC.gopt_set` GHC.Opt_UseBytecodeRatherThanObjects
          `GHC.gopt_set` GHC.Opt_InsertBreakpoints

    logger1 <- GHC.getLogger
    let logger2 = GHC.setLogFlags logger1 (GHC.initLogFlags dflags1)

          -- The rest of the arguments are "dynamic"
          -- Leftover ones are presumably files
    (dflags4, fileish_args, _dynamicFlagWarnings) <-
        GHC.parseDynamicFlags logger2 dflags1 (map (GHC.mkGeneralLocated "on ghc-debugger command arg") ghcInvocation)

    let (dflags5, srcs, objs) = GHC.parseTargetFiles dflags4 (map GHC.unLoc fileish_args)

    -- we've finished manipulating the DynFlags, update the session
    _ <- GHC.setSessionDynFlags dflags5

    dflags6 <- GHC.getSessionDynFlags

    -- Should this be done in GHC=
    liftIO $ GHC.initUniqSupply (GHC.initialUnique dflags6) (GHC.uniqueIncrement dflags6)

    -- Initialise plugins here because the plugin author might already expect this
    -- subsequent call to `getLogger` to be affected by a plugin.
    GHC.initializeSessionPlugins
    hsc_env <- GHC.getSession
    _logger <- GHC.getLogger

    ---------------- Final sanity checking -----------
    liftIO $ GHC.checkOptions GHC.DoInteractive dflags6 srcs objs units
    
    hs_srcs <- case NE.nonEmpty units of
      Just ne_units -> do
        GHC.initMulti ne_units
      Nothing -> do
        case srcs of
          [] -> return []
          _  -> do
            -- ROMES: think about where this initMake should go in GHC or if it should really be copied over.
            let (hs_srcs, non_hs_srcs) = List.partition GHC.isHaskellishTarget srcs

            -- if we have no haskell sources from which to do a dependency
            -- analysis, then just do one-shot compilation and/or linking.
            -- This means that "ghc Foo.o Bar.o -o baz" links the program as
            -- we expect.
            if (null hs_srcs)
               then liftIO (GHC.oneShot hsc_env GHC.NoStop srcs) >> return []
               else do
                 o_files <- GHC.mapMaybeM (\x -> liftIO $ GHC.compileFile hsc_env GHC.NoStop x) non_hs_srcs
                 dflags7 <- GHC.getSessionDynFlags
                 let dflags' = dflags7 { GHC.ldInputs = map (GHC.FileOption "") o_files ++ GHC.ldInputs dflags7 }
                 _ <- GHC.setSessionDynFlags dflags'
                 return $ map (uncurry (,Nothing,)) hs_srcs

    targets' <- mapM (\(src, uid, phase) -> GHC.guessTarget src uid phase) hs_srcs
    GHC.setTargets targets'
    ok_flag <- GHC.load GHC.LoadAllTargets
    when (GHC.failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

    -- TODO: Shouldn't initLoaderState be called somewhere?

    -- Set interactive context to import all loaded modules
    -- TODO: Think about Note [GHCi and local Preludes] and what is done in `getImplicitPreludeImports`
    let preludeImp = GHC.IIDecl . GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"
    mss <- getAllLoadedModules
    GHC.setContext $ preludeImp : map (GHC.IIDecl . GHC.simpleImportDecl . GHC.ms_mod_name) mss

    runReaderT action =<< initialDebuggerState

-- | Registers or deletes a breakpoint in the GHC session and from the list of
-- active breakpoints that is kept in 'DebuggerState', depending on the
-- 'BreakpointStatus' being set.
--
-- Returns @True@ when the breakpoint status is changed.
registerBreakpoint :: GHC.BreakpointId -> BreakpointStatus -> BreakpointKind -> Debugger Bool
registerBreakpoint bp@GHC.BreakpointId
                    { GHC.bi_tick_mod = mod
                    , GHC.bi_tick_index = bid } status kind = do

  -- Set breakpoint in GHC session
  let breakpoint_count = breakpointStatusInt status
  hsc_env <- GHC.getSession
  GHC.setupBreakpoint hsc_env bp breakpoint_count

  -- Register breakpoint in Debugger state
  brksRef <- asks activeBreakpoints
  oldBrks <- liftIO $ readIORef brksRef
  let
    (newBrks, changed) = case status of
      -- Disabling the breakpoint; using the `Maybe` monad:
      -- * If we reach the return stmt then the breakpoint is active and we delete it.
      -- * Any other case, return False and change Nothing
      BreakpointDisabled -> fromMaybe (oldBrks, False) $ do
        im <- lookupModuleEnv oldBrks mod
        _status <- IM.lookup bid im
        let im'  = IM.delete bid im
            brks = extendModuleEnv oldBrks mod im'
        return (brks, True)

      -- We're enabling the breakpoint:
      _ -> case lookupModuleEnv oldBrks mod of
        Nothing ->
          let im   = IM.singleton bid (status, kind)
              brks = extendModuleEnv oldBrks mod im
           in (brks, True)
        Just im -> case IM.lookup bid im of
          Nothing ->
            -- Not yet in IntMap, extend with new Breakpoint
            let im' = IM.insert bid (status, kind) im
                brks = extendModuleEnv oldBrks mod im'
             in (brks, True)
          Just (status', _kind) ->
            -- Found in IntMap
            if status' == status then
              (oldBrks, False)
            else
              let im'  = IM.insert bid (status, kind) im
                  brks = extendModuleEnv oldBrks mod im'
               in (brks, True)

  -- no races since the debugger execution is run in a single thread
  liftIO $ writeIORef brksRef newBrks
  return changed

-- | Get a list with all currently active breakpoints on the given module (by path)
--
-- If the path argument is @Nothing@, get all active function breakpoints instead
getActiveBreakpoints :: Maybe FilePath -> Debugger [GHC.BreakpointId]
getActiveBreakpoints mfile = do
  m <- asks activeBreakpoints >>= liftIO . readIORef
  case mfile of
    Just file -> do
      ms <- getModuleByPath file
      return $
        [ GHC.BreakpointId mod bix
        | (mod, im) <- moduleEnvToList m
        , mod == ms_mod ms
        , bix <- IM.keys im
        -- assert: status is always > disabled
        ]
    Nothing -> do
      return $
        [ GHC.BreakpointId mod bix
        | (mod, im) <- moduleEnvToList m
        , (bix, (status, kind)) <- IM.assocs im

        -- Keep only function breakpoints in this case
        , FunctionBreakpointKind == kind

        , assert (status > BreakpointDisabled) True
        ]

-- | List all loaded modules 'ModSummary's
getAllLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getAllLoadedModules =
  (GHC.mgModSummaries <$> GHC.getModuleGraph) >>=
    filterM (\ms -> GHC.isLoadedModule (GHC.ms_unitid ms) (GHC.ms_mod_name ms))

-- | Get a 'ModSummary' of a loaded module given its 'FilePath'
getModuleByPath :: FilePath -> Debugger ModSummary
getModuleByPath path = do
  -- do this everytime as the loaded modules may have changed
  lms <- getAllLoadedModules
  let matches ms = msHsFilePath ms `List.isSuffixOf` path
  case filter matches lms of
    [x] -> return x
    [] -> error $ "No Module matched " ++ path
    xs -> error $ "Too many modules (" ++ showPprUnsafe xs ++ ") matched " ++ path

--------------------------------------------------------------------------------
-- Variable references
--------------------------------------------------------------------------------

-- | Find a variable's associated Term and Name by reference ('Int')
lookupVarByReference :: Int -> Debugger (Maybe (Name, Term))
lookupVarByReference i = do
  ioref <- asks varReferences
  rm <- readIORef ioref & liftIO
  return $ IM.lookup i rm

-- | Inserts a mapping from the given variable reference to the variable's
-- associated Term and the Name it is bound to for display
insertVarReference :: Int -> Name -> Term -> Debugger ()
insertVarReference i name term = do
  ioref <- asks varReferences
  rm <- readIORef ioref & liftIO
  let
    rm' = IM.insert i (name, term) rm
  writeIORef ioref rm' & liftIO

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Evaluate a suspended Term to WHNF.
--
-- Used in @'getVariables'@ to reply to a variable introspection request.
seqTerm :: Term -> Debugger Term
seqTerm term = do
  hsc_env <- GHC.getSession
  let
    interp = hscInterp hsc_env
    unit_env = hsc_unit_env hsc_env
  case term of
    Suspension{val, ty} -> liftIO $ do
      r <- GHCi.seqHValue interp unit_env val
      () <- fromEvalResult r
      cvObtainTerm hsc_env 5 False ty val
    _ -> return term

-- | Resume execution with single step mode 'RunToCompletion', skipping all breakpoints we hit, until we reach 'ExecComplete'.
--
-- We use this in 'doEval' because we want to ignore breakpoints in expressions given at the prompt.
continueToCompletion :: Debugger GHC.ExecResult
continueToCompletion = do
  execr <- GHC.resumeExec GHC.RunToCompletion Nothing
  case execr of
    GHC.ExecBreak{} -> continueToCompletion
    GHC.ExecComplete{} -> return execr

-- | Turn a 'BreakpointStatus' into its 'Int' representation for 'BreakArray'
breakpointStatusInt :: BreakpointStatus -> Int
breakpointStatusInt = \case
  BreakpointEnabled      -> BA.breakOn  -- 0
  BreakpointDisabled     -> BA.breakOff -- -1
  BreakpointAfterCount n -> n           -- n

-- | Generate a new unique 'Int'
freshInt :: Debugger Int
freshInt = do
  ioref <- asks genUniq
  i <- readIORef ioref & liftIO
  let !i' = i+1
  writeIORef ioref i'  & liftIO
  return i

-- | Initialize a 'DebuggerState'
initialDebuggerState :: GHC.Ghc DebuggerState
initialDebuggerState = DebuggerState <$> liftIO (newIORef emptyModuleEnv)
                                     <*> liftIO (newIORef IM.empty)
                                     <*> liftIO (newIORef 0)

-- | Lift a 'Ghc' action into a 'Debugger' one.
liftGhc :: GHC.Ghc a -> Debugger a
liftGhc = Debugger . ReaderT . const

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance GHC.HasLogger Debugger where
  getLogger = liftGhc GHC.getLogger

instance GHC.GhcMonad Debugger where
  getSession = liftGhc GHC.getSession
  setSession s = liftGhc $ GHC.setSession s


