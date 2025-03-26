{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase, ScopedTypeVariables #-}
{-

== Overview

Main launches 3 lightweight threads.
- One thread listens to requests and writes them to a `Chan` (C1=requests)
- Another reads from `Chan` (C2=replies) and sends replies
- The worker thread reads requests from (C1), executes (e.g. by interpreting the debuggee program until a break), and writes responses to (C2)

┌────────────────────────────────────┐          
│Main                                │          
└┬────────────────────┬─────────────┬┘          
┌▽──────────────────┐┌▽───────────┐┌▽──────────┐
│Listen for requests││Send replies││GHC session│
└───────────────────┘└────────────┘└───────────┘


== Configuration

Currently, there is no support for changing the configuration of `ghc-debugger` at runtime.
- The `ghc-debugger` specific options are passed as process arguments as per 'Settings'.
- The GHC-specific flags that determine how to compile/interpret the project
  are passed after a double dash (@--@) in the list of arguments as well.
  Don't include MODE flags (like --interpreter)!

== Notes

For now, debugging is only supported in the interpreter mode.
When this changes, the code can be revised to accommodate those capabilities.
-}
module Main where

import Data.Functor
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad.Catch

import GHC.IO.Handle
import System.IO
import System.Environment (getArgs)

import Debugger
import Debugger.Monad
import Debugger.Interface

data Settings = Settings
      { --   logLevel?
        -- , force inspect?
        -- , context-modules?
        ghcInvocation :: [String]
      , libdir :: FilePath
      , units :: [String]
      }

main :: IO ()
main = do
  ghcInvocationFlags <- getArgs

  -- setLocaleEncoding utf8

  -- Duplicate @stdout@ as @hout@ and move @stdout@ to @stderr@. @hout@ still
  -- points to the standard output, which is now written exclusively by the sender.
  -- This guards against stray prints from corrupting the JSON-RPC message stream.
  hout <- hDuplicate stdout
  stderr `hDuplicateTo` stdout

  hSetBuffering hout LineBuffering
  hSetBuffering stdout LineBuffering -- ? the same handle as below?
  hSetBuffering stderr LineBuffering

  requests <- newChan
  replies  <- newChan

  _ <- forkIO $ receiver requests
  _ <- forkIO $ sender hout replies

  -- Run debugger in main thread
  debugger requests replies
    (mkSettings ghcInvocationFlags)

  return ()

-- | Make 'Settings' from ghc invocation flags
mkSettings :: [String] -> Settings
mkSettings flags = Settings
  { ghcInvocation = flags
  , libdir = fromMaybe (error "Libdir must be specified with -B!") $ listToMaybe $
             mapMaybe (\case '-':'B':dir -> Just dir; _ -> Nothing) flags
  , units  = mapMaybe (\case ("-unit", u) -> Just u; _ -> Nothing) $ zip flags (drop 1 flags)
  }

-- | The main worker. Runs a GHC session which executes 'Command's received from
-- the given @'Chan' 'Command'@ and writes 'Response's to the @'Chan' 'Response'@ channel.
debugger :: Chan Command -> Chan Response -> Settings -> IO ()
debugger requests replies Settings{libdir, units, ghcInvocation} =
  runDebugger libdir units ghcInvocation $
    forever $ do
      req <- liftIO $ readChan requests
      resp <- (execute req <&> Right)
                `catch` \(e :: SomeException) -> pure (Left (displayException e))
      either bad reply resp
  where
    reply = liftIO . writeChan replies
    bad m = liftIO $ do
      writeChan replies (Aborted m)

