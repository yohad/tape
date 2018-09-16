module Tape.Win32 where

import System.Win32.Types
import Foreign.C.String
import Foreign
import System.Win32.DebugApi

foreign import ccall "CreateDebuggedProcess"
    c_CreateDebuggedProcess :: LPCSTR -> IO PHANDLE

foreign import ccall "windows.h GetModuleFileNameA"
    c_GetModuleFileNameA :: HANDLE -> LPSTR -> DWORD -> IO DWORD

getModuleFileName :: HANDLE -> IO (Maybe String)
getModuleFileName moduleHandle = do
    let bufferSize = 2048 :: Int -- plenty, PATH_MAX is 512 under Win32.
    buffer <- mallocArray bufferSize
    returnValue <- c_GetModuleFileNameA moduleHandle buffer $ fromIntegral bufferSize
    if 0 == returnValue
        then do
            lastError <- c_GetLastError
            print lastError
            return Nothing
        else Just `fmap` peekCString buffer

foreign import ccall "GetLastError"
        c_GetLastError :: IO DWORD

foreign import ccall "GetFinalPathNameByHandleA"
    c_GetFinalPathNameByHandleA :: HANDLE -> LPSTR -> DWORD -> DWORD -> IO DWORD

getFinalPathNameByHandle :: HANDLE -> IO (Maybe String)
getFinalPathNameByHandle fileHandle = allocaBytes bufferSize $ \buffer -> do
    returnValue <- c_GetFinalPathNameByHandleA fileHandle buffer (fromIntegral bufferSize) 0
    if 0 == returnValue
        then return Nothing
        else Just `fmap` peekCString buffer
    where 
        bufferSize = 2048  