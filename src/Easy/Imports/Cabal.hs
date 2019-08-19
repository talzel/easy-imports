{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
module Easy.Imports.Cabal where

import "base"  Data.Maybe
import "Cabal" Distribution.PackageDescription
import "Cabal" Distribution.PackageDescription.Parsec
import "Cabal" Distribution.Types.Dependency
import "Cabal" Distribution.Types.PackageName
import "Cabal" Distribution.Types.VersionRange
import "lens"  Control.Lens
import qualified "bytestring" Data.ByteString as B
import qualified "containers" Data.Set as Set
import "pretty-simple"        Text.Pretty.Simple

updateCabalFile :: FilePath -> [String] -> IO ()
updateCabalFile fp _packages = do
    putStrLn fp
    content <- B.readFile fp
    let (_warnings, res) = runParseResult (parseGenericPackageDescription content)
    pkgDesc <- case res of
        Right pd -> return pd -- TODO: handle warnings
        Left err -> error $ show err
    -- get library dependencies
    -- unionfy them
    -- update and save
    pPrint pkgDesc

getDependencies :: GenericPackageDescription -> [Dependency]
getDependencies GenericPackageDescription{..} =
    -- TODO: more clarification on the condTree http://hackage.haskell.org/package/Cabal-3.0.0.0/docs/Distribution-Types-CondTree.html#t:CondTree
    let libDeps = fromMaybe [] $ fmap condTreeConstraints condLibrary -- TODO: needs to verify that this covers everything when pulling lib deps
        --exeDepends = map condTreeConstraints condExecutables
    in libDeps

updateDependencies :: GenericPackageDescription -> [Dependency] -> GenericPackageDescription
updateDependencies gpd@GenericPackageDescription{condLibrary=cl} deps = gpd{condLibrary=cl'}
  where
      cl' = fmap worker cl
      worker :: (CondTree ConfVar [Dependency] Library) -> (CondTree ConfVar [Dependency] Library)
      worker ct@CondNode{condTreeData=cta} = ct{ condTreeData=cta'
                                      , condTreeConstraints = deps
                                      }
          where
          cta' = cta -- finish implementing code


newDependency :: String -> Dependency
newDependency nm = Dependency (mkPackageName nm) anyVersion Set.empty