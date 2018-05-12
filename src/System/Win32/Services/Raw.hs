{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module System.Win32.Services.Raw where

import Import
import System.Win32.Services.Status
import System.Win32.Services.TableEntry

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

type HANDLER_FUNCTION_EX = DWORD -> DWORD -> Ptr () -> Ptr () -> IO DWORD

foreign import WINDOWS_CCONV "wrapper"
    smfToFunPtr :: SERVICE_MAIN_FUNCTION -> IO (FunPtr SERVICE_MAIN_FUNCTION)

foreign import WINDOWS_CCONV "wrapper"
    handlerToFunPtr :: HANDLER_FUNCTION_EX -> IO (FunPtr HANDLER_FUNCTION_EX)

-- BOOL WINAPI QueryServiceStatus(
--   _In_   SC_HANDLE hService,
--   _Out_  LPSERVICE_STATUS lpServiceStatus
-- );
foreign import WINDOWS_CCONV "windows.h QueryServiceStatus"
    c_QueryServiceStatus :: HANDLE -> Ptr ServiceStatus -> IO BOOL

-- I've not been able to get RegisterServiceCtrlHandler to work on Windows 7 64-bit.
foreign import WINDOWS_CCONV "windows.h RegisterServiceCtrlHandlerExW"
    c_RegisterServiceCtrlHandlerEx
        :: LPTSTR -> FunPtr HANDLER_FUNCTION_EX -> Ptr () -> IO HANDLE

foreign import WINDOWS_CCONV "windows.h SetServiceStatus"
    c_SetServiceStatus :: HANDLE -> Ptr ServiceStatus -> IO BOOL

foreign import WINDOWS_CCONV "windows.h StartServiceCtrlDispatcherW"
    c_StartServiceCtrlDispatcher :: Ptr ServiceTableEntry -> IO BOOL
