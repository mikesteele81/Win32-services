{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.Services.Raw where

import Import
import System.Win32.Services.Status
import System.Win32.Services.TableEntry

type HANDLER_FUNCTION_EX = DWORD -> DWORD -> Ptr () -> Ptr () -> IO DWORD

foreign import stdcall "wrapper"
    smfToFunPtr :: SERVICE_MAIN_FUNCTION -> IO (FunPtr SERVICE_MAIN_FUNCTION)

foreign import stdcall "wrapper"
    handlerToFunPtr :: HANDLER_FUNCTION_EX -> IO (FunPtr HANDLER_FUNCTION_EX)

-- BOOL WINAPI QueryServiceStatus(
--   _In_   SC_HANDLE hService,
--   _Out_  LPSERVICE_STATUS lpServiceStatus
-- );
foreign import stdcall "windows.h QueryServiceStatus"
    c_QueryServiceStatus :: HANDLE -> Ptr SERVICE_STATUS -> IO BOOL

-- I've not been able to get RegisterServiceCtrlHandler to work on Windows 7 64-bit.
foreign import stdcall "windows.h RegisterServiceCtrlHandlerExW"
    c_RegisterServiceCtrlHandlerEx
        :: LPTSTR -> FunPtr HANDLER_FUNCTION_EX -> Ptr () -> IO HANDLE

foreign import stdcall "windows.h SetServiceStatus"
    c_SetServiceStatus :: HANDLE -> Ptr SERVICE_STATUS -> IO BOOL

foreign import stdcall "windows.h StartServiceCtrlDispatcherW"
    c_StartServiceCtrlDispatcher :: Ptr SERVICE_TABLE_ENTRY -> IO BOOL
