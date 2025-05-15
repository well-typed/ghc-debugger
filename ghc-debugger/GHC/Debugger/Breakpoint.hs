{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger.Breakpoint where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Bits (xor)

import GHC
import GHC.Utils.Error (logOutput)
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Ppr as GHC
import GHC.Runtime.Debugger.Breakpoints as GHC
import GHC.Unit.Module.Env as GHC
import GHC.Utils.Outputable as GHC

import GHC.Debugger.Monad
import GHC.Debugger.Utils
import GHC.Debugger.Interface.Messages

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

-- | Find a 'BreakpointId' and its span from a module + line + column.
--
-- Used by 'setBreakpoints' and 'GetBreakpointsAt' requests
getBreakpointsAt :: ModSummary {-^ module -} -> Int {-^ line num -} -> Maybe Int {-^ column num -} -> Debugger (Maybe (BreakIndex, RealSrcSpan))
getBreakpointsAt modl lineNum columnNum = do
  -- TODO: Cache moduleLineMap.
  mticks <- makeModuleLineMap (ms_mod modl)
  let mbid = do
        ticks <- mticks
        case columnNum of
          Nothing -> findBreakByLine lineNum ticks
          Just col -> findBreakByCoord (lineNum, col) ticks
  return mbid

-- | Set a breakpoint in this session
setBreakpoint :: Breakpoint -> BreakpointStatus -> Debugger BreakFound
setBreakpoint ModuleBreak{path, lineNum, columnNum} bp_status = do
  mmodl <- getModuleByPath path
  case mmodl of
    Left e -> do
      displayWarnings [e]
      return BreakNotFound
    Right modl -> do
      mbid <- getBreakpointsAt modl lineNum columnNum

      case mbid of
        Nothing -> return BreakNotFound
        Just (bix, spn) -> do
          let bid = BreakpointId { bi_tick_mod = ms_mod modl
                                 , bi_tick_index = bix }
          changed <- registerBreakpoint bid bp_status ModuleBreakpointKind
          return $ BreakFound
            { changed = changed
            , sourceSpan = realSrcSpanToSourceSpan spn
            , breakId = bid
            }
setBreakpoint FunctionBreak{function} bp_status = do
  logger <- getLogger
  resolveFunctionBreakpoint function >>= \case
    Left e -> error (showPprUnsafe e)
    Right (modl, mod_info, fun_str) -> do
      let modBreaks = GHC.modInfoModBreaks mod_info
          applyBreak (bix, spn) = do
            let bid = BreakpointId { bi_tick_mod = modl
                                   , bi_tick_index = bix }
            changed <- registerBreakpoint bid bp_status FunctionBreakpointKind
            return $ BreakFound
              { changed = changed
              , sourceSpan = realSrcSpanToSourceSpan spn
              , breakId = bid
              }
      case findBreakForBind fun_str modBreaks of
        []  -> do
          liftIO $ logOutput logger (text $ "No breakpoint found by name " ++ function ++ ". Ignoring...")
          return BreakNotFound
        [b] -> applyBreak b
        bs  -> do
          liftIO $ logOutput logger (text $ "Ambiguous breakpoint found by name " ++ function ++ ": " ++ show bs ++ ". Setting breakpoints in all...")
          ManyBreaksFound <$> mapM applyBreak bs
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
