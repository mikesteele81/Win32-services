module System.Win32.Services.Control
    ( ServiceControl (..)
    , peekServiceControl
    , pokeServiceControl
    , fromDWORD
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
peekServiceControl ptr = fromDWORD <$> peek ptr

pokeServiceControl :: Ptr DWORD -> ServiceControl -> IO ()
pokeServiceControl ptr sc = poke ptr . toDWORD $ sc

toDWORD :: ServiceControl -> DWORD
toDWORD Continue       = 0x00000003
toDWORD Interrogate    = 0x00000004
toDWORD NetBindAdd     = 0x00000007
toDWORD NetBindDisable = 0x0000000A
toDWORD NetBindEnable  = 0x00000009
toDWORD NetBindRemove  = 0x00000008
toDWORD ParamChange    = 0x00000006
toDWORD Pause          = 0x00000002
toDWORD PreShutdown    = 0x0000000F
toDWORD Shutdown       = 0x00000005
toDWORD Stop           = 0x00000001

fromDWORD :: DWORD -> Either String ServiceControl
fromDWORD 0x00000003 = Right Continue
fromDWORD 0x00000004 = Right Interrogate
fromDWORD 0x00000007 = Right NetBindAdd
fromDWORD 0x0000000A = Right NetBindDisable
fromDWORD 0x00000009 = Right NetBindEnable
fromDWORD 0x00000008 = Right NetBindRemove
fromDWORD 0x00000006 = Right ParamChange
fromDWORD 0x00000002 = Right Pause
fromDWORD 0x0000000F = Right PreShutdown
fromDWORD 0x00000005 = Right Shutdown
fromDWORD 0x00000001 = Right Stop
fromDWORD 0x0000000B = unsupported "SERVICE_CONTROL_DEVICEEVENT"
fromDWORD 0x0000000C = unsupported "SERVICE_CONTROL_HARDWAREPROFILECHANGE"
fromDWORD 0x0000000D = unsupported "SERVICE_CONTROL_POWEREVENT"
fromDWORD 0x0000000E = unsupported "SERVICE_CONTROL_SESSIONCHANGE"
fromDWORD 0x00000010 = unsupported "SERVICE_CONTROL_TIMECHANGE"
fromDWORD 0x00000020 = unsupported "SERVICE_CONTROL_TRIGGEREVENT"
fromDWORD 0x00000040 = unsupported "SERVICE_CONTROL_USERMODEREBOOT"
fromDWORD x
    | x >= 128 && x <= 255 = Left "user defined control codes are unsupported by this binding."
    | otherwise = Left $ "The " ++ printf "%x" x ++ " control code is undocumented."

unsupported :: String -> Either String a
unsupported name = Left $ "The " ++ name ++ " control code is unsupported by this binding."
