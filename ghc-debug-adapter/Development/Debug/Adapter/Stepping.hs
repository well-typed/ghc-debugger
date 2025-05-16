module Development.Debug.Adapter.Stepping where

import DAP

import GHC.Debugger.Interface.Messages hiding (Command, Response)

import Development.Debug.Adapter
import Development.Debug.Adapter.Interface
import Development.Debug.Adapter.Evaluation

commandContinue :: DebugAdaptor ()
commandContinue = do
  DidContinue er <- sendInterleaved DoContinue $
    sendContinueResponse (ContinueResponse True)
  handleEvalResult False er

commandNext :: DebugAdaptor ()
commandNext = do
  DidStep er <- sendInterleaved DoStepLocal sendNextResponse
  handleEvalResult True er

commandStepIn :: DebugAdaptor ()
commandStepIn = do
  DidStep er <- sendInterleaved DoSingleStep sendStepInResponse
  handleEvalResult True er

commandStepOut :: DebugAdaptor ()
commandStepOut = do
  DidStep er <- sendInterleaved DoStepOut sendStepOutResponse
  handleEvalResult True er

