{-# LANGUAGE CPP #-}
module Tape.Win32 where

import System.Win32.Types
import Foreign.C.String
import Foreign
import System.Win32.DebugApi

-- In Win32 the max is 512, So I think it is safe enough
#define MAX_FILE_PATH (1024)

foreign import ccall "CreateDebuggedProcess"
    c_CreateDebuggedProcess :: LPCSTR -> IO PHANDLE

foreign import ccall "windows.h GetModuleFileNameA"
    c_GetModuleFileNameA :: HANDLE -> LPSTR -> DWORD -> IO DWORD

getModuleFileName :: HANDLE -> IO (Maybe String)
getModuleFileName moduleHandle = allocaBytes MAX_FILE_PATH $ \buffer -> do
    returnValue <- c_GetModuleFileNameA moduleHandle buffer $ fromIntegral MAX_FILE_PATH
    if 0 == returnValue
        then return Nothing
        else Just `fmap` peekCString buffer

foreign import ccall "GetLastError"
        c_GetLastError :: IO DWORD

foreign import ccall "GetFinalPathNameByHandleA"
    c_GetFinalPathNameByHandleA :: HANDLE -> LPSTR -> DWORD -> DWORD -> IO DWORD

getFinalPathNameByHandle :: HANDLE -> IO (Maybe String)
getFinalPathNameByHandle fileHandle = allocaBytes MAX_FILE_PATH $ \buffer -> do
    returnValue <- c_GetFinalPathNameByHandleA fileHandle buffer (fromIntegral MAX_FILE_PATH) 0
    if 0 == returnValue
        then return Nothing
        else Just `fmap` peekCString buffer