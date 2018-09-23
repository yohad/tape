module Main where

import Control.Monad.State.Lazy

import System.Win32.DebugApi

import Data.Map.Strict

import Tape.Repl
import Tape.Types

main :: IO ()
main = do
    _ <- runStateT repl $ DebugEnvironment Nothing ((0, 0), UnknownDebugEvent) empty -- TODO: (0,0) is MAGIC, replace it
    return ()