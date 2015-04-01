module System.Win32.Services.Control
    ( ServiceControl (..)
    , peekServiceControl
    , pokeServiceControl
    , marshIn
    ) where

import Text.Printf

import Import

-- | A ServiceControl is used in Handler functions. All control codes are
--   defined here, but some can only be used with a 'HandlerEx' callback.
--   Use 'convertSuccess' to translate from a 'ServiceControl' to a 'DWORD'.
--   Use 'convertAttempt' to translate from a 'DWORD' to a 'ServiceControl'.
data ServiceControl = Continue | Interrogate | NetBindAdd | NetBindDisable
    | NetBindEnable | NetBindRemove | ParamChange | Pause
    | PreShutdown | Shutdown | Stop
    deriving (Show)

peekServiceControl :: Ptr DWORD -> IO (Either String ServiceControl)
peekServiceControl ptr = marshIn <$> peek ptr

pokeServiceControl :: Ptr DWORD -> ServiceControl -> IO ()
pokeServiceControl ptr sc = poke ptr . marshOut $ sc

-- | Marshal a ServiceAccept "out" to be used in C-land
marshOut :: ServiceControl -> DWORD
marshOut Continue       = 0x00000003
marshOut Interrogate    = 0x00000004
marshOut NetBindAdd     = 0x00000007
marshOut NetBindDisable = 0x0000000A
marshOut NetBindEnable  = 0x00000009
marshOut NetBindRemove  = 0x00000008
marshOut ParamChange    = 0x00000006
marshOut Pause          = 0x00000002
marshOut PreShutdown    = 0x0000000F
marshOut Shutdown       = 0x00000005
marshOut Stop           = 0x00000001

-- | Marshall a DWORD "in" to be used in Haskell-land as a ServiceAccept
marshIn :: DWORD -> Either String ServiceControl
marshIn 0x00000003 = Right Continue
marshIn 0x00000004 = Right Interrogate
marshIn 0x00000007 = Right NetBindAdd
marshIn 0x0000000A = Right NetBindDisable
marshIn 0x00000009 = Right NetBindEnable
marshIn 0x00000008 = Right NetBindRemove
marshIn 0x00000006 = Right ParamChange
marshIn 0x00000002 = Right Pause
marshIn 0x0000000F = Right PreShutdown
marshIn 0x00000005 = Right Shutdown
marshIn 0x00000001 = Right Stop
marshIn 0x0000000B = unsupported "SERVICE_CONTROL_DEVICEEVENT"
marshIn 0x0000000C = unsupported "SERVICE_CONTROL_HARDWAREPROFILECHANGE"
marshIn 0x0000000D = unsupported "SERVICE_CONTROL_POWEREVENT"
marshIn 0x0000000E = unsupported "SERVICE_CONTROL_SESSIONCHANGE"
marshIn 0x00000010 = unsupported "SERVICE_CONTROL_TIMECHANGE"
marshIn 0x00000020 = unsupported "SERVICE_CONTROL_TRIGGEREVENT"
marshIn 0x00000040 = unsupported "SERVICE_CONTROL_USERMODEREBOOT"
marshIn x
    | x >= 128 && x <= 255 = Left "user defined control codes are unsupported by this binding."
    | otherwise = Left $ "The " ++ printf "%x" x ++ " control code is undocumented."

unsupported :: String -> Either String a
unsupported name = Left $ "The " ++ name ++ " control code is unsupported by this binding."
