module Main where

import qualified Easy.Imports.CLI as CLI
import Easy.Imports

main :: IO ()
main = CLI.parser >>= run
