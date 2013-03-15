module System.Win32.SystemServices.Services.SERVICE_CONTROL
    ( SERVICE_CONTROL (..)
    , peekServiceControl
    , pokeServiceControl
    , fromDWORD
    ) where

import Foreign
import System.Win32.Types (DWORD)
import Text.Printf

import Control.Error

-- | A SERVICE_CONTROL is used in Handler functions. All control codes are
--   defined here, but some can only be used with a 'HandlerEx' callback.
--   Use 'convertSuccess' to translate from a 'SERVICE_CONTROL' to a 'DWORD'.
--   Use 'convertAttempt' to translate from a 'DWORD' to a 'SERVICE_CONTROL'.
data SERVICE_CONTROL = CONTINUE | INTERROGATE | NETBINDADD | NETBINDDISABLE
    | NETBINDENABLE | NETBINDREMOVE | PARAMCHANGE | PAUSE
    | PRESHUTDOWN | SHUTDOWN | STOP
    deriving (Show)

peekServiceControl :: Ptr DWORD -> IO SERVICE_CONTROL
peekServiceControl ptr = runScript $ do
    dword <- scriptIO $ peek ptr
    hoistEither $ fromDWORD dword

pokeServiceControl :: Ptr DWORD -> SERVICE_CONTROL -> IO ()
pokeServiceControl ptr sc = poke ptr . toDWORD $ sc

toDWORD :: SERVICE_CONTROL -> DWORD
toDWORD CONTINUE       = 0x00000003
toDWORD INTERROGATE    = 0x00000004
toDWORD NETBINDADD     = 0x00000007
toDWORD NETBINDDISABLE = 0x0000000A
toDWORD NETBINDENABLE  = 0x00000009
toDWORD NETBINDREMOVE  = 0x00000008
toDWORD PARAMCHANGE    = 0x00000006
toDWORD PAUSE          = 0x00000002
toDWORD PRESHUTDOWN    = 0x0000000F
toDWORD SHUTDOWN       = 0x00000005
toDWORD STOP           = 0x00000001

fromDWORD :: DWORD -> Either String SERVICE_CONTROL
fromDWORD 0x00000003 = Right CONTINUE
fromDWORD 0x00000004 = Right INTERROGATE
fromDWORD 0x00000007 = Right NETBINDADD
fromDWORD 0x0000000A = Right NETBINDDISABLE
fromDWORD 0x00000009 = Right NETBINDENABLE
fromDWORD 0x00000008 = Right NETBINDREMOVE
fromDWORD 0x00000006 = Right PARAMCHANGE
fromDWORD 0x00000002 = Right PAUSE
fromDWORD 0x0000000F = Right PRESHUTDOWN
fromDWORD 0x00000005 = Right SHUTDOWN
fromDWORD 0x00000001 = Right STOP
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
