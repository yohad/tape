module Tape.Debug where

import System.Win32.DebugApi
import System.Win32.Types

import Foreign.C.String
import Foreign.ForeignPtr

import Data.Map.Strict
import Data.Functor ((<$>))

import Control.Monad.State.Lazy
import Control.Lens

import Text.Printf

import Tape.Win32
import Tape.Types

createDebuggedProcess :: String -> IO HANDLE
createDebuggedProcess path = 
    withCString path c_CreateDebuggedProcess

processDebugEvent :: DebugEvent -> App ()
processDebugEvent (_, DebugString stringAddress isUnicode stringSize) = do
    processHandle <- view debuggeeHandle <$> get
    case processHandle of
        Just process -> do
            cString <- liftIO $ readProcessMemory process stringAddress $ fromIntegral stringSize
            if isUnicode then liftIO $ withForeignPtr cString $ \ptrString -> do
                debugString <- peekCWString ptrString
                putStrLn debugString
            else 
                liftIO $ putStrLn "DebugString in ASCII not supported"
        Nothing -> return ()
    
processDebugEvent (_, CreateProcess{}) = do
    filepath <- liftIO . getModuleFileName $ nullPtr
    case filepath of
        Just filename -> liftIO . putStrLn $ "Created process: " ++ filename
        Nothing -> liftIO . putStrLn $ "CreateProcess: Unkown error while getting file name"
    
processDebugEvent (_, LoadDll (handle, dllBase, _, _, _)) = do
    filepath <- liftIO . getFinalPathNameByHandle $ handle
    case filepath of
        Just filename -> do
            put =<< over loadedDlls (insert dllBase filename) <$> get
            liftIO . putStrLn $ "LoadDll: " ++ filename
        Nothing -> liftIO . putStrLn $ "Invalid DLL handle"

processDebugEvent (_, CreateThread (_, threadStartRoutine, threadId)) =
    liftIO $ printf "Thread <threadHandle> (Id: %d) created at: 0x%x\n" threadId threadStartRoutine

processDebugEvent (debugEventId, ExitThread exitCode) =
    liftIO $ printf "The thread %d exited with code: %d\n" (fst debugEventId) exitCode

processDebugEvent (_, UnloadDll baseOfDll) = do
    dllname <- Data.Map.Strict.lookup baseOfDll . view loadedDlls <$> get 
    case dllname of
        Just name -> liftIO . putStrLn  $ "Unloaded DLL: " ++ name
        Nothing -> liftIO . putStrLn $ "Unloaded unregister dll (Base: " ++ show baseOfDll ++ ")"

processDebugEvent (_, ExitProcess exitCode) =
    liftIO . putStrLn $ "Process exited with code: " ++ show exitCode

processDebugEvent unknownDebugEvent = liftIO $ print unknownDebugEvent
