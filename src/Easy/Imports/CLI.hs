{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE LambdaCase        #-}
module Easy.Imports.CLI where

import "optparse-generic" Options.Generic
import "haskell-src-exts" Language.Haskell.Exts
import "base"             Data.List
import "directory"        System.Directory
import "base"             Control.Monad
import "filepath"         System.FilePath
import "base"             Data.Maybe
import "base"             Control.Exception
import "base"             System.IO.Error

data Cmd = Stack { packageDirectory :: FilePath}
    deriving (Generic, Show)

instance ParseRecord Cmd

cliParser :: IO Cmd
cliParser = getRecord "Easy Imports"

main = do
    x <- cliParser
    run x

run :: Cmd -> IO ()
run (Stack fp) = do
    isHaskellPackage fp
    files <- listAllFiles fp
    let haskellFiles = filter isHaskellModule files
        mPackageYamlFile = listToMaybe $ filter isPackageYaml files
    packageYaml <- case mPackageYamlFile of
        Nothing -> return $ error $ "No stack file in package: " ++ show fp
        Just f  -> return f
    packages <- fmap (sort . nub . concat ) $ forM haskellFiles $ \hsFile -> do
        parseFile hsFile >>= \case
            ParseFailed err2 err -> error $ show [err, show err2]
            ParseOk m -> return $ getImports m
    let packages' = removeBaseModule packages
    updatePackageYaml packageYaml packages'
    -- todo: update cabal from stack automatically

updatePackageYaml fp packages = do
    contents <- readFile fp
    let contents' = modifyPackagesSection packages contents
    putStrLn contents'

getImports (Module _ _mMh pragmas importDecls _)
    | hasPackageImports pragmas = mapMaybe importPkg importDecls
    | otherwise = []
getImports mdl =  error $ "unsupported module Type: " ++ show mdl

hasPackageImports :: [ModulePragma SrcSpanInfo] -> Bool
hasPackageImports = any isPackageImport
    where
        isPackageImport (LanguagePragma _ ns) = any isPackageImportsLbl ns
        isPackageImport l = error $ "unsupported imports case :" ++ show l
        isPackageImportsLbl (Ident _ err) = err == "PackageImports"
        isPackageImportsLbl l = error $ "unsupported feature:" ++ show l



isHaskellModule = isSuffixOf ".hs"

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

isHiddenFolder :: [Char] -> Bool
isHiddenFolder path =
    let (_, fileName) = splitFileName path
    in isPrefixOf "." fileName

isHaskellPackage :: FilePath -> IO Bool
isHaskellPackage path = do
    actualFiles <- listFiles path
    return $ any isPackageYaml actualFiles

listFiles :: FilePath -> IO [FilePath]
listFiles path =
    map (path </>) <$> listDirectory path >>=
        filterM (fmap not . doesDirectoryExist)
listFolders' :: FilePath -> IO [[Char]]
listFolders' fp = filter (not . isHiddenFolder) <$> listFolders fp

listFolders :: FilePath -> IO [FilePath]
listFolders path =
    map (path </>) <$> listDirectory path >>=
        filterM doesDirectoryExist

isPackageYaml :: [Char] -> Bool
isPackageYaml = isSuffixOf "package.yaml"
isCabalFile :: [Char] -> Bool
isCabalFile = isSuffixOf ".cabal"

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles dirpath = do
    directoryFiles <- map (dirpath </>) <$> listDirectory dirpath
    actualFiles <- filterM (fmap not . doesDirectoryExist) directoryFiles
    folders <- filterM doesDirectoryExist directoryFiles
    let folders' = filter (not . isHiddenFolder) folders
    subfiles <- mapM listAllFiles folders'
    return $ reverse $ actualFiles ++ concat subfiles

modifyPackagesSection :: [String] -> String -> String
modifyPackagesSection packages fileContent =
    let unyamlListElem = drop 2
        yamlListElem = (++) "- "
        fileLines = lines fileContent
        dependencieslist = map unyamlListElem $ getDependenciesBlock fileLines
        newDependenciesList = sort $ packages `union` dependencieslist
        newDependenciesList' = map yamlListElem newDependenciesList
        fileHead = reverse $ dropWhile (not . isDependenciesHdr) $ reverse fileLines
        fileTail = dropWhile isYamlListElem $ tail $ dropWhile (not . isDependenciesHdr) fileLines
    in unlines $ fileHead ++ newDependenciesList' ++ fileTail

getDependenciesBlock :: [String] -> [String]
getDependenciesBlock = takeWhile isYamlListElem . tail . dropWhile (not . isDependenciesHdr)

isYamlListElem :: [Char] -> Bool
isYamlListElem = isPrefixOf "- "

isDependenciesHdr :: String -> Bool
isDependenciesHdr = isPrefixOf "dependencies:"

removeBaseModule :: [String] -> [String]
removeBaseModule = filter ("base" /=)