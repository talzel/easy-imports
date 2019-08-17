{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE LambdaCase        #-}
module Easy.Imports.Cabal where

import "Cabal" Distribution.PackageDescription
import "Cabal" Distribution.PackageDescription.Parsec
import "Cabal" Distribution.Types.Dependency
import "Cabal" Distribution.Types.PackageName
import "Cabal" Distribution.Types.VersionRange
import qualified "bytestring" Data.ByteString as B
import qualified "containers" Data.Set as Set

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

getDependencies :: PackageDescription -> [Dependencies]
getDependencies = condToDeps . condLibrary
    where
        condToDeps = undefined





updateDependencies :: PackageDescription -> [Dependencies] -> PackageDescription
updateDependencies = unefined


newDependency :: String -> Dependency
newDependency nm = Dependency (mkPackageName nm) anyVersion Set.empty