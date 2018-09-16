module Main where

import System.Win32.DebugApi
import System.Win32.Types

import Foreign.C.String
import Foreign.ForeignPtr

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Text.Printf

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
processDebugEvent (DebugString stringAddress isUnicode stringSize) = do
    environment <- ask
    let processHandle = debuggeeHandle environment 
    cString <- liftIO $ readProcessMemory processHandle stringAddress $ fromIntegral stringSize
    if isUnicode then liftIO $ withForeignPtr cString $ \ptrString -> do
        debugString <- peekCWString ptrString
        putStrLn debugString
    else 
        liftIO $ putStrLn "DebugString in ASCII not supported"
 
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

processDebugEvent (CreateThread (_, threadStartRoutine, threadId)) =
    liftIO $ printf "Thread <threadHandle> (Id: %d) created at: 0x%x\n" threadId threadStartRoutine

processDebugEvent unknownDebugEvent = liftIO $ print unknownDebugEvent
