{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
-- {-# LANGUAGE TemplateHaskell   #-}

module Easy.Imports.Cabal where

import "base"  Data.Maybe
import "Cabal" Distribution.PackageDescription
import "Cabal" Distribution.PackageDescription.Parsec
import "Cabal" Distribution.PackageDescription.PrettyPrint
import qualified "Cabal" Distribution.Types.GenericPackageDescription.Lens as L
import "Cabal" Distribution.Types.Dependency
import "Cabal" Distribution.Types.PackageName
import "Cabal" Distribution.Types.VersionRange
import "lens"  Control.Lens
import qualified "bytestring" Data.ByteString as B
import qualified "containers" Data.Set as Set
import "pretty-simple"        Text.Pretty.Simple
import                        Easy.Imports.TH


updateCabalFile :: FilePath -> [String] -> IO ()
updateCabalFile fp _packages = do
    putStrLn fp
    content <- B.readFile fp
    let (_warnings, res) = runParseResult (parseGenericPackageDescription content)
    pkgDesc <- case res of
        Right pd -> return pd -- TODO: handle warnings
        Left err -> error $ show err
    -- get library dependencies
    let deps = getDependencies pkgDesc
    -- unionfy them
    -- update and save

    pPrint deps
    --pPrint $ updateDependencies pkgDesc []
    --writeGenericPackageDescription "tal.cabal" $ updateDependencies pkgDesc []

getDependencies :: GenericPackageDescription -> [Dependency]
getDependencies gpd =
    gpd ^. (L.condLibrary . traversed . condTreeConstraintsLens)


updateDependencies :: GenericPackageDescription -> [Dependency] -> GenericPackageDescription
updateDependencies gpd@GenericPackageDescription{condLibrary=cl} deps = gpd{condLibrary=cl'}
    where
        cl' = fmap worker cl
        worker :: (CondTree ConfVar [Dependency] Library) -> (CondTree ConfVar [Dependency] Library)
        worker ct@CondNode{condTreeData=cta} = ct{ condTreeData= f cta
                                                , condTreeConstraints = deps
                                                }
        f :: Library -> Library
        f cta@Library{libBuildInfo = lbi} = cta{libBuildInfo = g lbi}
        g :: BuildInfo -> BuildInfo
        g lbi = lbi{targetBuildDepends = deps}



newDependency :: String -> Dependency
newDependency nm = Dependency (mkPackageName nm) anyVersion