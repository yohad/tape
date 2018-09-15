{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.Win32.Types
import System.Win32.DebugApi
import Foreign.C.String
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

newtype DebugEnvironment = DebugEnvironment { processHandle :: HANDLE }
type App = ReaderT DebugEnvironment IO

main :: IO ()
main = do
    putStrLn "Started"
    processHandle <- withCString "C:\\Windows\\System32\\calc.exe" c_CreateDebuggedProcess
    putStrLn "created process"
    runReaderT debugLoop $ DebugEnvironment processHandle

debugLoop :: App ()
debugLoop = do
    maybeDebugEvent <- liftIO $ waitForDebugEvent Nothing -- Defualt is INFINITE
    case maybeDebugEvent of
        Nothing -> liftIO $ putStrLn "DebugEvent: Nothing"
        Just debugEvent@(debugEventId, _) -> do
            processDebugEvent debugEvent
            liftIO $ continueDebugEvent debugEventId True 
            debugLoop

processDebugEvent :: DebugEvent -> App ()
processDebugEvent (debugEventInfo, DebugString foreignAddress isUnicode stringLength) = do
    environment <- ask
    let handle = processHandle environment
    liftIO $ putChar 'd'

processDebugEvent a = liftIO . print $ a


foreign import ccall "CreateDebuggedProcess"
    c_CreateDebuggedProcess :: LPCSTR -> IO HANDLE