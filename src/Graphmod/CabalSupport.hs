module Graphmod.CabalSupport (parseCabalFile,Unit(..),UnitName(..)) where

import Graphmod.Utils(ModName,fromHierarchy)

import Data.Maybe(maybeToList)
import System.FilePath((</>))

-- Interface to cabal.
import Distribution.Verbosity(silent)
import Distribution.PackageDescription
        ( GenericPackageDescription, PackageDescription(..)
        , Library(..), Executable(..), BuildInfo(..) )
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.ModuleName(ModuleName,components)
import Distribution.Utils.Path (getSymbolicPath, makeSymbolicPath)
import Distribution.Simple.PackageDescription(readGenericPackageDescription)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Pretty (prettyShow)

pretty :: UnqualComponentName -> String
pretty = prettyShow

parseCabalFile :: FilePath -> IO [Unit]
parseCabalFile f = fmap findUnits (readGenericPackageDescription silent Nothing $ makeSymbolicPath f)


-- | This is our abstraction for something in a cabal file.
data Unit = Unit
  { unitName    :: UnitName
  , unitPaths   :: [FilePath]
  , unitModules :: [ModName]
  , unitFiles   :: [FilePath]
  } deriving Show

data UnitName = UnitLibrary | UnitExecutable String
                deriving Show


libUnit :: Library -> Unit
libUnit lib = Unit { unitName     = UnitLibrary
                   , unitPaths    = getSymbolicPath <$> hsSourceDirs (libBuildInfo lib)
                   , unitModules  = map toMod (exposedModules lib)
                                                      -- other modules?
                   , unitFiles    = []
                   }

exeUnit :: Executable -> Unit
exeUnit exe = Unit { unitName    = UnitExecutable (pretty $ exeName exe)
                   , unitPaths   = getSymbolicPath <$> hsSourceDirs (buildInfo exe)
                   , unitModules = [] -- other modules?
                   , unitFiles   = case hsSourceDirs (buildInfo exe) of
                                     [] -> [ getSymbolicPath $ modulePath exe ]
                                     ds -> [ getSymbolicPath d </> getSymbolicPath (modulePath exe) | d <- ds ]
                   }

toMod :: ModuleName -> ModName
toMod m = case components m of
            [] -> error "Empty module name."
            xs -> (fromHierarchy (init xs), last xs)

findUnits :: GenericPackageDescription -> [Unit]
findUnits g = maybeToList (fmap libUnit (library pkg))  ++
                           fmap exeUnit (executables pkg)
  where
  pkg = flattenPackageDescription g -- we just ignore flags
