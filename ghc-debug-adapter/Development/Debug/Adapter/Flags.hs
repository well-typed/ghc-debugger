{-# LANGUAGE CPP, RecordWildCards, LambdaCase #-}
module Development.Debug.Adapter.Flags where

import Data.Function
import System.FilePath
import System.Directory
import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.List as L

import qualified HIE.Bios as HIE
import qualified HIE.Bios.Types as HIE
import qualified HIE.Bios.Environment as HIE

-- | Flags inferred by @hie-bios@ to invoke GHC
data HieBiosFlags = HieBiosFlags
      { ghcInvocation :: [String]
      , libdir :: FilePath
      , units :: [String]
      }

-- | Make 'HieBiosFlags' from the given target file
hieBiosFlags :: FilePath {-^ Project root -} -> FilePath {-^ Entry file relative to root -} -> IO (Either String HieBiosFlags)
hieBiosFlags root relTarget = runExceptT $ do

  let target = root </> relTarget

  explicitCradle <- HIE.findCradle target & liftIO
  -- TODO: Use proper loggers here
  cradle <- maybe (HIE.loadImplicitCradle mempty target)
                  (HIE.loadCradle mempty) explicitCradle & liftIO

  libdir <- liftIO (HIE.getRuntimeGhcLibDir cradle) >>= unwrapCradleResult "Failed to get runtime GHC libdir"

  -- To determine the flags we MUST set the current directory to the root
  -- because hie.yaml may invoke programs relative to the root (e.g. GHC's hie.yaml does)
  -- (HIE.getCompilerOptions depends on CWD being the proper root dir)
  let compilerOpts = liftIO $ withCurrentDirectory root $
#if MIN_VERSION_hie_bios(0,14,0)
                          HIE.getCompilerOptions target (HIE.LoadWithContext [target]) cradle
#else
                          HIE.getCompilerOptions target [] cradle
#endif
  HIE.ComponentOptions {HIE.componentOptions = flags} <- compilerOpts >>= unwrapCradleResult "Failed to get compiler options using hie-bios cradle"

#if __GLASGOW_HASKELL__ >= 913
  -- fwrite-if-simplified-core requires a recent bug fix regarding GHCi loading
  -- ROMES:TODO: Re-enable as soon as I'm using Matthew's patch.
  -- ["-fwrite-if-simplified-core"] ++
#endif

  let (units', flags') = extractUnits flags
  return HieBiosFlags
    { ghcInvocation =  -- TODO is this correct? else, the debugger won't work on single files.
                      flags' ++ ghcDebuggerFlags
    , libdir = libdir
    , units  = units'
    }
  where
    unwrapCradleResult m = \case
      HIE.CradleNone     -> throwError $ "HIE.CradleNone\n" ++ m
      HIE.CradleFail err -> throwError $ unlines (HIE.cradleErrorStderr err) ++ "\n" ++ m
      HIE.CradleSuccess x -> return x

extractUnits :: [String] -> ([String], [String])
extractUnits = go [] []
  where
    go units rest ("-unit" : x : xs) = go (x : units) rest xs
    go units rest (x : xs)           = go units (x : rest) xs
    go units rest []                 = (reverse units, reverse rest)

-- | Flags specific to ghc-debugger to append to all GHC invocations.
ghcDebuggerFlags :: [String]
ghcDebuggerFlags =
  [ "-fno-it" -- don't introduce @it@ after evaluating something at the prompt
  ]
