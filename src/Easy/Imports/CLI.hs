{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE LambdaCase        #-}
module Easy.Imports.CLI where

import "optparse-generic" Options.Generic


data Cmd = Stack { packageDirectory :: FilePath
                 , debug            :: Bool
                 , updateCabal      :: Bool
                 }
         | Cabal { packageDirectory :: FilePath
                 , debug            :: Bool
                 }

    deriving (Generic, Show)

instance ParseRecord Cmd

parser :: IO Cmd
parser = getRecord "Easy Imports"

