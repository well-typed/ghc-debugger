{-# LANGUAGE DerivingStrategies, CPP, RecordWildCards #-}

-- | Initialise the GHC session for one or more home units.
--
-- This code is inspired of HLS's session initialisation.
-- It would be great to extract common functions in the future.
module GHC.Debugger.Session (
  parseHomeUnitArguments,
  setupHomeUnitGraph,
  TargetDetails(..),
  Target(..),
  toGhcTarget,
  CacheDirs(..),
  getCacheDirs,
  -- * DynFlags modifications
  setWorkingDirectory,
  setCacheDirs,
  writeInterfaces,
  setBytecodeBackend,
  enableByteCodeGeneration,
  )
  where

import Control.Monad
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                    as H
import qualified Data.ByteString.Base16              as B16
import qualified Data.ByteString.Char8               as B
import Data.Function
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Containers.ListUtils as L
import GHC.ResponseFile (expandResponse)
import HIE.Bios.Environment as HIE
import System.FilePath
import qualified System.Directory as Directory
import qualified System.Environment as Env

import qualified GHC
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Monad
import qualified GHC.Driver.Session as GHC
import GHC.Utils.Monad as GHC
import GHC.Unit.Home.Graph
import GHC.Unit.Home.PackageTable
import GHC.Unit.Env
import GHC.Unit.Types
import qualified GHC.Unit.State                        as State
import GHC.Driver.Env
import GHC.Types.SrcLoc
import Language.Haskell.Syntax.Module.Name

-- | Throws if package flags are unsatisfiable
parseHomeUnitArguments :: GhcMonad m
    => FilePath -- ^ Main entry point function
    -> FilePath -- ^ Component root. Important for multi-package cabal projects.
    -> [String]
    -> [String] -- ghcInvocation
    -> DynFlags
    -> FilePath -- ^ root dir, see Note [Root Directory]
    -> m (NonEmpty.NonEmpty (DynFlags, [GHC.Target]))
parseHomeUnitArguments cfp compRoot units theOpts dflags rootDir = do
    ((theOpts',_errs,_warns),_units) <- GHC.processCmdLineP [] [] (map noLoc theOpts)
    case NonEmpty.nonEmpty units of
      Just us -> initMulti us
      Nothing -> do
        (df, targets) <- initOne (map unLoc theOpts')
        -- A special target for the file which caused this wonderful
        -- component to be created. In case the cradle doesn't list all the targets for
        -- the component, in which case things will be horribly broken anyway.
        --
        -- When we have a singleComponent that is caused to be loaded due to a
        -- file, we assume the file is part of that component. This is useful
        -- for bare GHC sessions, such as many of the ones used in the testsuite
        --
        -- We don't do this when we have multiple components, because each
        -- component better list all targets or there will be anarchy.
        -- It is difficult to know which component to add our file to in
        -- that case.
        -- Multi unit arguments are likely to come from cabal, which
        -- does list all targets.
        --
        -- If we don't end up with a target for the current file in the end, then
        -- we will report it as an error for that file
        let abs_fp = rootDir </> cfp
        let special_target = mkSimpleTarget df abs_fp
        pure $ (df, special_target : targets) NonEmpty.:| []
    where
      initMulti unitArgFiles =
        forM unitArgFiles $ \f -> do
          args <- liftIO $ expandResponse [f]
          initOne args
      initOne this_opts = do
        (dflags', targets') <- addCmdOpts this_opts dflags
        let targets = HIE.makeTargetsAbsolute root targets'
            root = case workingDirectory dflags' of
              Nothing   -> compRoot
              Just wdir -> compRoot </> wdir
        cacheDirs <- liftIO $ getCacheDirs (takeFileName root) this_opts
        let dflags'' =
              setWorkingDirectory root $
              setCacheDirs cacheDirs $
              enableByteCodeGeneration $
              setBytecodeBackend $
              makeDynFlagsAbsolute compRoot -- makeDynFlagsAbsolute already accounts for workingDirectory
              dflags'
        return (dflags'', targets)

setupHomeUnitGraph :: GhcMonad m => [(DynFlags, [GHC.Target])] -> m ()
setupHomeUnitGraph flagsAndTargets = do
  hsc_env <- GHC.getSession
  (hsc_env', targetDetails) <- liftIO $ setupMultiHomeUnitGhcSession [".hs", ".lhs"] hsc_env flagsAndTargets
  GHC.setSession hsc_env'
  GHC.setTargets (fmap toGhcTarget targetDetails)

-- | Set up the 'HomeUnitGraph' with empty 'HomeUnitEnv's.
createUnitEnvFromFlags :: NonEmpty.NonEmpty DynFlags -> IO HomeUnitGraph
createUnitEnvFromFlags unitDflags = do
  let
    newInternalUnitEnv dflags hpt = mkHomeUnitEnv State.emptyUnitState Nothing dflags hpt Nothing

  unitEnvList <- traverse (\dflags -> do
    emptyHpt <- emptyHomePackageTable
    pure (homeUnitId_ dflags, newInternalUnitEnv dflags emptyHpt)) unitDflags

  pure $ unitEnv_new (Map.fromList (NonEmpty.toList unitEnvList))

-- | Given a set of 'DynFlags', set up the 'UnitEnv' and 'HomeUnitEnv' for this
-- 'HscEnv'.
-- We assume the 'HscEnv' is "empty", e.g. wasn't already used to compile
-- anything.
initHomeUnitEnv :: [DynFlags] -> HscEnv -> IO HscEnv
initHomeUnitEnv unitDflags env = do
  let dflags0         = hsc_dflags env
  -- additionally, set checked dflags so we don't lose fixes
  initial_home_graph <- createUnitEnvFromFlags (dflags0 NonEmpty.:| unitDflags)
  let home_units = unitEnv_keys initial_home_graph
  home_unit_graph <- forM initial_home_graph $ \homeUnitEnv -> do
    let cached_unit_dbs = homeUnitEnv_unit_dbs homeUnitEnv
        dflags = homeUnitEnv_dflags homeUnitEnv
        old_hpt = homeUnitEnv_hpt homeUnitEnv

    (dbs,unit_state,home_unit,mconstants) <- State.initUnits (hsc_logger env) dflags cached_unit_dbs home_units

    updated_dflags <- GHC.updatePlatformConstants dflags mconstants
    pure HomeUnitEnv
      { homeUnitEnv_units = unit_state
      , homeUnitEnv_unit_dbs = Just dbs
      , homeUnitEnv_dflags = updated_dflags
      , homeUnitEnv_hpt = old_hpt
      , homeUnitEnv_home_unit = Just home_unit
      }

  let dflags1 = homeUnitEnv_dflags $ unitEnv_lookup (homeUnitId_ dflags0) home_unit_graph
  let unit_env = UnitEnv
        { ue_platform        = targetPlatform dflags1
        , ue_namever         = GHC.ghcNameVersion dflags1
        , ue_home_unit_graph = home_unit_graph
        , ue_current_unit    = homeUnitId_ dflags0
        , ue_module_graph    = ue_module_graph (hsc_unit_env env)
        , ue_eps             = ue_eps (hsc_unit_env env)
        }
  pure $ hscSetFlags dflags1 $ hscSetUnitEnv unit_env env


-- | Setup the given 'HscEnv' to hold a 'UnitEnv'
-- with all the given components.
-- We return the modified 'HscEnv' and all the 'TargetDetails' for
-- the given 'GHC.Target's.
setupMultiHomeUnitGhcSession
         :: [String]           -- ^ File extensions to consider. This is mostly a remnant of HLS.
         -> HscEnv             -- ^ An empty HscEnv that we can use the setup the session.
         -> [(DynFlags, [GHC.Target])]    -- ^ New components to be loaded. Expected to be non-empty.
         -> IO (HscEnv, [TargetDetails])
setupMultiHomeUnitGhcSession exts hsc_env cis = do
    let dfs = map fst cis

    hscEnv' <- initHomeUnitEnv dfs hsc_env
    -- TODO: this should be reported
    -- _ <- maybeToList $ GHC.checkHomeUnitsClosed (hsc_unit_env hscEnv') (hsc_all_home_unit_ids hscEnv')
    ts <- forM cis $ \(df, targets) -> do
      -- evaluate $ liftRnf rwhnf targets

      let mk t = fromTargetId (importPaths df) exts (homeUnitId_ df) (GHC.targetId t)
      ctargets <- concatMapM mk targets

      return (L.nubOrdOn targetTarget ctargets)
    pure (hscEnv', concat ts)

data TargetDetails = TargetDetails
  { targetTarget :: Target
  -- ^ Simplified version of 'TargetId', storing enough information
  --
  , targetLocations :: [FilePath]
  -- ^ The physical location of 'targetTarget'.
  -- Contains '-boot' file locations.
  -- At this moment in time, these are unused, but could be used to create
  -- convenient lookup table from 'FilePath' to 'TargetDetails'.
  , targetUnitId :: UnitId
  -- ^ UnitId of 'targetTarget'.
  }
  deriving (Eq, Ord)

-- | A simplified view on a 'TargetId'.
--
-- Implements 'Ord' and 'Show' which can be convenient.
data Target = TargetModule ModuleName | TargetFile FilePath
  deriving ( Eq, Ord, Show )

-- | Turn a 'TargetDetails' into a 'GHC.Target'.
toGhcTarget :: TargetDetails -> GHC.Target
toGhcTarget (TargetDetails tid _ uid) = case tid of
  TargetModule modl -> GHC.Target (GHC.TargetModule modl) True uid Nothing
  TargetFile fp -> GHC.Target (GHC.TargetFile fp Nothing) True uid Nothing

fromTargetId :: [FilePath]          -- ^ import paths
             -> [String]            -- ^ extensions to consider
             -> UnitId
             -> GHC.TargetId
             -> IO [TargetDetails]
-- For a target module we consider all the import paths
fromTargetId is exts unitId (GHC.TargetModule modName) = do
    let fps = [i </> moduleNameSlashes modName -<.> ext <> boot
              | ext <- exts
              , i <- is
              , boot <- ["", "-boot"]
              ]
    return [TargetDetails (TargetModule modName) fps unitId]
-- For a 'TargetFile' we consider all the possible module names
fromTargetId _ _ unitId (GHC.TargetFile f _) = do
    let other
          | "-boot" `L.isSuffixOf` f = dropEnd 5 f
          | otherwise = (f ++ "-boot")
    return [TargetDetails (TargetFile f) [f, other] unitId]

-- ----------------------------------------------------------------------------
-- GHC Utils that should likely be exposed by GHC
-- ----------------------------------------------------------------------------

mkSimpleTarget :: DynFlags -> FilePath -> GHC.Target
mkSimpleTarget df fp = GHC.Target (GHC.TargetFile fp Nothing) True (homeUnitId_ df) Nothing

hscSetUnitEnv :: UnitEnv -> HscEnv -> HscEnv
hscSetUnitEnv ue env = env { hsc_unit_env = ue }

-- ----------------------------------------------------------------------------
-- Session cache directory
-- ----------------------------------------------------------------------------

data CacheDirs = CacheDirs
  { hiCacheDir :: FilePath
  , byteCodeCacheDir :: FilePath
  , hieCacheDir :: FilePath
  , objCacheDir :: FilePath
  }

getCacheDirs :: String -> [String] -> IO CacheDirs
getCacheDirs prefix opts = do
  mCacheDir <- Env.lookupEnv "HDB_CACHE_DIR"
  rootDir <- case mCacheDir of
    Just dir -> pure dir
    Nothing ->
      Directory.getXdgDirectory Directory.XdgCache "hdb"
  let sessionCacheDir = rootDir </> prefix ++ "-" ++ opts_hash
  Directory.createDirectoryIfMissing True sessionCacheDir
  pure CacheDirs
    { hiCacheDir = sessionCacheDir
    , byteCodeCacheDir = sessionCacheDir
    , hieCacheDir = sessionCacheDir
    , objCacheDir = sessionCacheDir
    }
  where
    -- Create a unique folder per set of different GHC options, assuming that each different set of
    -- GHC options will create incompatible interface files.
    opts_hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init (map B.pack opts)

-- ----------------------------------------------------------------------------
-- Modification of DynFlags
-- ----------------------------------------------------------------------------

setWorkingDirectory :: FilePath -> DynFlags -> DynFlags
setWorkingDirectory p d = d { workingDirectory =  Just p }

setCacheDirs :: CacheDirs -> DynFlags -> DynFlags
setCacheDirs CacheDirs{..} flags = flags
  { hiDir = Just hiCacheDir
  , hieDir = Just hieCacheDir
  , objectDir = Just objCacheDir
#if MIN_VERSION_ghc(9,14,2)
  , bytecodeDir = Just byteCodeCacheDir
#endif
  }

enableByteCodeGeneration :: DynFlags -> DynFlags
enableByteCodeGeneration dflags =
#if MIN_VERSION_ghc(9,14,2)
  dflags
    & flip gopt_unset Opt_ByteCodeAndObjectCode
    & flip gopt_set Opt_ByteCode
    & flip gopt_set Opt_WriteByteCode
#else
  gopt_set dflags Opt_WriteIfSimplifiedCore
#endif

writeInterfaces :: DynFlags -> DynFlags
writeInterfaces dflags =
  gopt_set dflags Opt_WriteInterface

setBytecodeBackend :: DynFlags -> DynFlags
setBytecodeBackend dflags = dflags
  {
#if MIN_VERSION_ghc(9,14,2)
  backend = GHC.bytecodeBackend
#else
  backend = GHC.interpreterBackend
#endif
  }

-- ----------------------------------------------------------------------------
-- Utils that we need, but don't want to incur an additional dependency for.
-- ----------------------------------------------------------------------------

-- | Drop a number of elements from the end of the list.
--
-- > dropEnd 3 "hello"  == "he"
-- > dropEnd 5 "bye"    == ""
-- > dropEnd (-1) "bye" == "bye"
-- > \i xs -> dropEnd i xs `isPrefixOf` xs
-- > \i xs -> length (dropEnd i xs) == max 0 (length xs - max 0 i)
-- > \i -> take 3 (dropEnd 5 [i..]) == take 3 [i..]
dropEnd :: Int -> [a] -> [a]
dropEnd i xs
    | i <= 0 = xs
    | otherwise = f xs (drop i xs)
    where f (a:as) (_:bs) = a : f as bs
          f _ _ = []
