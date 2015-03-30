module System.Win32.SystemServices.Services.Types where

import Import

type HANDLER_FUNCTION_EX = DWORD -> DWORD -> Ptr () -> Ptr () -> IO DWORD
type LPHANDLER_FUNCTION_EX = FunPtr HANDLER_FUNCTION_EX

type SERVICE_MAIN_FUNCTION = DWORD -> Ptr LPTSTR -> IO ()
type LPSERVICE_MAIN_FUNCTION = FunPtr SERVICE_MAIN_FUNCTION
