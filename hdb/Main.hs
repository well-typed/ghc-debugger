{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, CPP, DeriveAnyClass, DeriveGeneric, DerivingVia, LambdaCase, RecordWildCards, ViewPatterns #-}
module Main where

import System.Environment
import Data.Maybe
import Text.Read

import DAP

import Development.Debug.Adapter.Init
import Development.Debug.Adapter.Breakpoints
import Development.Debug.Adapter.Stepping
import Development.Debug.Adapter.Stopped
import Development.Debug.Adapter.Evaluation
import Development.Debug.Adapter.Exit
import Development.Debug.Adapter.Handles
import Development.Debug.Adapter.Logger
import Development.Debug.Adapter

import System.IO (hSetBuffering, BufferMode(LineBuffering))
import DAP.Log
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Handle.FD

defaultStdoutForwardingAction :: T.Text -> IO ()
defaultStdoutForwardingAction line = do
  T.hPutStrLn stderr ("[INTERCEPTED STDOUT] " <> line)

main :: IO ()
main = do
  port <- getArgs >>= \case
            ["--server", "--port", readMaybe -> Just p] -> return p
            _ -> fail "usage: --server --port <port>"
  config <- getConfig port
  withInterceptedStdoutForwarding defaultStdoutForwardingAction $ \realStdout -> do
    hSetBuffering realStdout LineBuffering
    l <- handleLogger realStdout
    let
      timeStampLogger = cmapIO renderWithTimestamp l
      loggerWithSev = cmap (renderWithSeverity id) timeStampLogger
    runDAPServerWithLogger (cmap renderDAPLog timeStampLogger) config (talk loggerWithSev)

-- | Fetch config from environment, fallback to sane defaults
getConfig :: Int -> IO ServerConfig
getConfig port = do
  let
    hostDefault = "0.0.0.0"
    portDefault = port
    capabilities = defaultCapabilities
      { -- Exception breakpoints!
        exceptionBreakpointFilters            = [ defaultExceptionBreakpointsFilter
                                                  { exceptionBreakpointsFilterLabel = "All exceptions"
                                                  , exceptionBreakpointsFilterFilter = BREAK_ON_EXCEPTION
                                                  }
                                                , defaultExceptionBreakpointsFilter
                                                  { exceptionBreakpointsFilterLabel = "Uncaught exceptions"
                                                  , exceptionBreakpointsFilterFilter = BREAK_ON_ERROR
                                                  }
                                                ]
        -- Function breakpoints!
      , supportsFunctionBreakpoints           = True

      , supportsEvaluateForHovers             = False

      -- display which breakpoints are valid when user intends to set
      -- breakpoint on given line.
      , supportsBreakpointLocationsRequest    = True
      , supportsConfigurationDoneRequest      = True
      , supportsHitConditionalBreakpoints     = False
      , supportsModulesRequest                = False
      , additionalModuleColumns               = [ defaultColumnDescriptor
                                                  { columnDescriptorAttributeName = "Extra"
                                                  , columnDescriptorLabel = "Label"
                                                  }
                                                ]
      , supportsValueFormattingOptions        = True
      , supportTerminateDebuggee              = True
      , supportsLoadedSourcesRequest          = False
      , supportsExceptionOptions              = True
      , supportsExceptionFilterOptions        = False
      }
  ServerConfig
    <$> do fromMaybe hostDefault <$> lookupEnv "DAP_HOST"
    <*> do fromMaybe portDefault . (readMaybe =<<) <$> do lookupEnv "DAP_PORT"
    <*> pure capabilities
    <*> pure True

--------------------------------------------------------------------------------
-- * Talk
--------------------------------------------------------------------------------

-- | Main function where requests are received and Events + Responses are returned.
-- The core logic of communicating between the client <-> adaptor <-> debugger
-- is implemented in this function.
talk :: LogAction IO (WithSeverity T.Text) -> Command -> DebugAdaptor ()
--------------------------------------------------------------------------------
talk l = \ case
  CommandInitialize -> do
    -- InitializeRequestArguments{..} <- getArguments
    sendInitializeResponse
--------------------------------------------------------------------------------
  CommandLaunch -> do
    success <- initDebugger l =<< getArguments
    if success then do
      sendLaunchResponse   -- ack
      sendInitializedEvent -- our debugger is only ready to be configured after it has launched the session
    else
      sendError ErrorMessageCancelled Nothing
--------------------------------------------------------------------------------
  CommandAttach -> undefined
--------------------------------------------------------------------------------
  CommandBreakpointLocations       -> commandBreakpointLocations
  CommandSetBreakpoints            -> commandSetBreakpoints
  CommandSetFunctionBreakpoints    -> commandSetFunctionBreakpoints
  CommandSetExceptionBreakpoints   -> commandSetExceptionBreakpoints
  CommandSetDataBreakpoints        -> undefined
  CommandSetInstructionBreakpoints -> undefined
----------------------------------------------------------------------------
  CommandLoadedSources -> undefined
----------------------------------------------------------------------------
  CommandConfigurationDone -> do
    sendConfigurationDoneResponse
    -- now that it has been configured, start executing until it halts, then send an event
    startExecution >>= handleEvalResult False
----------------------------------------------------------------------------
  CommandThreads    -> commandThreads
  CommandStackTrace -> commandStackTrace
  CommandScopes     -> commandScopes
  CommandVariables  -> commandVariables
----------------------------------------------------------------------------
  CommandContinue   -> commandContinue
----------------------------------------------------------------------------
  CommandNext       -> commandNext
----------------------------------------------------------------------------
  CommandStepIn     -> commandStepIn
  CommandStepOut    -> commandStepOut
----------------------------------------------------------------------------
  CommandEvaluate   -> commandEvaluate
----------------------------------------------------------------------------
  CommandTerminate  -> commandTerminate
  CommandDisconnect -> commandDisconnect
----------------------------------------------------------------------------
  CommandModules -> sendModulesResponse (ModulesResponse [] Nothing)
  CommandSource -> undefined
  CommandPause -> undefined
  (CustomCommand "mycustomcommand") -> undefined
----------------------------------------------------------------------------
-- talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------
