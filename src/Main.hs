module Main where

import CLI (runCLI)
import System.Environment (getArgs)
import System.Exit (exitWith)

main :: IO ()
main = getArgs >>= runCLI >>= exitWith
