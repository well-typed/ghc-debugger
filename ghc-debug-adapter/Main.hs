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
            ["--port", readMaybe -> Just p] -> return p
            _ -> fail "usage: --port <port>"
  config <- getConfig port
  withInterceptedStdoutForwarding defaultStdoutForwardingAction $ \realStdout -> do
    hSetBuffering realStdout LineBuffering
    l <- handleLogger realStdout
    runDAPServerWithLogger (cmap renderDAPLog l) config talk

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
talk :: Command -> DebugAdaptor ()
--------------------------------------------------------------------------------
talk CommandInitialize = do
  -- InitializeRequestArguments{..} <- getArguments
  sendInitializeResponse
--------------------------------------------------------------------------------
talk CommandLaunch = do
  success <- initDebugger =<< getArguments
  if success then do
    sendLaunchResponse   -- ack
    sendInitializedEvent -- our debugger is only ready to be configured after it has launched the session
  else
    sendError ErrorMessageCancelled Nothing
--------------------------------------------------------------------------------
talk CommandAttach = undefined
--------------------------------------------------------------------------------
talk CommandBreakpointLocations       = commandBreakpointLocations
talk CommandSetBreakpoints            = commandSetBreakpoints
talk CommandSetFunctionBreakpoints    = commandSetFunctionBreakpoints
talk CommandSetExceptionBreakpoints   = commandSetExceptionBreakpoints
talk CommandSetDataBreakpoints        = undefined
talk CommandSetInstructionBreakpoints = undefined
----------------------------------------------------------------------------
talk CommandLoadedSources = undefined
----------------------------------------------------------------------------
talk CommandConfigurationDone = do
  sendConfigurationDoneResponse
  -- now that it has been configured, start executing until it halts, then send an event
  startExecution >>= handleEvalResult False
----------------------------------------------------------------------------
talk CommandThreads    = commandThreads
talk CommandStackTrace = commandStackTrace
talk CommandScopes     = commandScopes
talk CommandVariables  = commandVariables
----------------------------------------------------------------------------
talk CommandContinue   = commandContinue
----------------------------------------------------------------------------
talk CommandNext       = commandNext
----------------------------------------------------------------------------
talk CommandStepIn     = commandStepIn
talk CommandStepOut    = commandStepOut
----------------------------------------------------------------------------
talk CommandEvaluate   = commandEvaluate
----------------------------------------------------------------------------
talk CommandTerminate  = commandTerminate
talk CommandDisconnect = commandDisconnect
----------------------------------------------------------------------------
talk CommandModules = sendModulesResponse (ModulesResponse [] Nothing)
talk CommandSource = undefined
talk CommandPause = undefined
talk (CustomCommand "mycustomcommand") = undefined
----------------------------------------------------------------------------
-- talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------
