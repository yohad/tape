{-# LANGUAGE TemplateHaskell #-}

module Tape.Types where

import Control.Monad.State.Lazy
import Control.Lens

import Data.Map.Strict

import System.Win32.DebugApi
import System.Win32.Types

-- When a breakpoint is met, this is the context which the REPL handle
data DebugEnvironment = DebugEnvironment { 
    _debuggeeHandle :: Maybe HANDLE
  , _lastEvent :: DebugEvent
  , _loadedDlls :: Map ForeignAddress String
}
makeLenses ''DebugEnvironment

type App = StateT DebugEnvironment IO