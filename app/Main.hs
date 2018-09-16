module Main where

import System.Win32.DebugApi
import System.Win32.Types
import Foreign.C.String
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Tape.Win32

newtype DebugEnvironment = DebugEnvironment { debuggeeHandle :: PHANDLE }
type App = ReaderT DebugEnvironment IO

main :: IO ()
main = do
    processHandle <- withCString "C:\\Users\\yotam\\AppData\\Roaming\\local\\bin\\hello.exe" c_CreateDebuggedProcess
    runReaderT debugLoop $ DebugEnvironment processHandle

debugLoop :: App ()
debugLoop = do
    maybeDebugEvent <- liftIO $ waitForDebugEvent Nothing -- Defualt is INFINITE
    case maybeDebugEvent of
        Nothing -> liftIO $ putStrLn "DebugEvent: Nothing "
        Just (debugEventId, debugEventInfo) -> do
            processDebugEvent debugEventInfo
            liftIO $ continueDebugEvent debugEventId True 
            debugLoop

processDebugEvent :: DebugEventInfo -> App ()
processDebugEvent CreateProcess{} = do
    filepath <- liftIO . getModuleFileName $ nullPtr
    case filepath of
        Just filename -> liftIO . putStrLn $ "Created process: " ++ filename
        Nothing -> liftIO . putStrLn $ "CreateProcess: Unkown error while getting file name"
    
processDebugEvent (LoadDll (handle, _, _, _, _)) = do
    filepath <- liftIO . getFinalPathNameByHandle $ handle
    case filepath of
        Just filename -> liftIO . putStrLn $ "LoadDll: " ++ filename
        Nothing -> liftIO . putStrLn $ "Invalid DLL handle"

processDebugEvent d = liftIO $ print d