{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.SystemServices.Services.Raw where

import Foreign.Ptr (Ptr)
import System.Win32.Types

import System.Win32.SystemServices.Services.SERVICE_STATUS
import System.Win32.SystemServices.Services.SERVICE_TABLE_ENTRY
import System.Win32.SystemServices.Services.Types

foreign import stdcall "wrapper"
    smfToFunPtr :: SERVICE_MAIN_FUNCTION -> IO LPSERVICE_MAIN_FUNCTION

foreign import stdcall "wrapper"
    handlerToFunPtr :: HANDLER_FUNCTION_EX -> IO LPHANDLER_FUNCTION_EX

-- BOOL WINAPI QueryServiceStatus(
--   _In_   SC_HANDLE hService,
--   _Out_  LPSERVICE_STATUS lpServiceStatus
-- );
foreign import stdcall "windows.h QueryServiceStatus"
    c_QueryServiceStatus :: HANDLE -> Ptr SERVICE_STATUS -> IO BOOL

-- I've not been able to get RegisterServiceCtrlHandler to work on Windows 7 64-bit.
foreign import stdcall "windows.h RegisterServiceCtrlHandlerExW"
    c_RegisterServiceCtrlHandlerEx :: LPTSTR -> LPHANDLER_FUNCTION_EX  -> Ptr () -> IO HANDLE

foreign import stdcall "windows.h SetServiceStatus"
    c_SetServiceStatus :: HANDLE -> Ptr SERVICE_STATUS -> IO BOOL

foreign import stdcall "windows.h StartServiceCtrlDispatcherW"
    c_StartServiceCtrlDispatcher :: Ptr SERVICE_TABLE_ENTRY -> IO BOOL
