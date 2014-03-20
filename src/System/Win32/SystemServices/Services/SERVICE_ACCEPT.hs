-- We refer to otherwise unused modules in documentation.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module System.Win32.SystemServices.Services.SERVICE_ACCEPT
    ( SERVICE_ACCEPT (..)
    , pokeServiceAccept
    , peekServiceAccept
    ) where

import Data.Bits
import Data.Maybe
import Text.Printf

import Import

-- Imported for haddocks
import qualified System.Win32.SystemServices.Services.SERVICE_CONTROL as C

-- | The control codes the service accepts and processes in its handler
--   function (See 'HandlerFunction'). By default, all services accept the
--   'C.INTERROGATE' value. To accept the 'DEVICEEVENT' value, the service must
--   register to receive device events by using the
--   'registerDeviceNotification' function.
data SERVICE_ACCEPT
    -- | The service is a network component that can accept changes in its
    --   binding without being stopped and restarted. This control code allows
    --   the service to receive 'C.NETBINDADD', 'C.NETBINDREMOVE',
    --   'C.NETBINDENABLE', and 'C.NETBINDDISABLE' notifications.
    = ACCEPT_NETBINDCHANGE
    -- | The service can reread its startup parameters without being stopped
    --   and restarted. This control code allows the service to receive
    --   'C.PARAMCHANGE' notifications.
    | ACCEPT_PARAMCHANGE
    -- | The service can be paused and continued. This control code allows the
    --   service to receive 'C.PAUSE' and 'C.CONTINUE' notifications.
    | ACCEPT_PAUSE_CONTINUE
    -- | MSDN documentation says that this function is not supported on
    --   Windows Server 2003 or Windows XP/2000. The support status on other
    --   versions is unknown to me.
    --
    --   The service can perform preshutdown tasks. This control code enables
    --   the service to receive 'C.PRESHUTDOWN' notifications.
    --   Note that only the system can send it.
    | ACCEPT_PRESHUTDOWN
    -- | The service is notified when system shutdown occurs. This control
    --   code allows the service to receive 'C.SHUTDOWN' notifications. Note
    --   that only the system can send it.
    | ACCEPT_SHUTDOWN
    -- | The service can be stopped. This control code allows the service to
    --   receive 'C.STOP' notifications.
    | ACCEPT_STOP
    deriving (Show)

peekServiceAccept :: Ptr DWORD -> IO [SERVICE_ACCEPT]
peekServiceAccept ptr = unflag <$> peek ptr

pokeServiceAccept :: Ptr DWORD -> [SERVICE_ACCEPT] -> IO ()
pokeServiceAccept ptr sas = poke ptr . flag $ sas

toDWORD :: SERVICE_ACCEPT -> DWORD
toDWORD ACCEPT_NETBINDCHANGE  = 0x00000010
toDWORD ACCEPT_PARAMCHANGE    = 0x00000008
toDWORD ACCEPT_PAUSE_CONTINUE = 0x00000002
toDWORD ACCEPT_PRESHUTDOWN    = 0x00000100
toDWORD ACCEPT_SHUTDOWN       = 0x00000004
toDWORD ACCEPT_STOP           = 0x00000001

fromDWORD :: DWORD -> Either String SERVICE_ACCEPT
fromDWORD 0x00000010 = Right ACCEPT_NETBINDCHANGE
fromDWORD 0x00000008 = Right ACCEPT_PARAMCHANGE
fromDWORD 0x00000002 = Right ACCEPT_PAUSE_CONTINUE
fromDWORD 0x00000100 = Right ACCEPT_PRESHUTDOWN
fromDWORD 0x00000004 = Right ACCEPT_SHUTDOWN
fromDWORD 0x00000001 = Right ACCEPT_STOP
fromDWORD 0x00000020 = unsupported "SERVICE_ACCEPT_HARDWAREPROFILECHANGE"
fromDWORD 0x00000040 = unsupported "SERVICE_ACCEPT_POWEREVENT"
fromDWORD 0x00000080 = unsupported "SERVICE_ACCEPT_SESSIONCHANGE"
fromDWORD 0x00000200 = unsupported "SERVICE_ACCEPT_TIMECHANGE"
fromDWORD 0x00000400 = unsupported "SERVICE_ACCEPT_TRIGGEREVENT"
fromDWORD 0x00000800 = unsupported "SERVICE_ACCEPT_USERMODEREBOOT"
fromDWORD x = Left $ "The " ++ printf "%x" x ++ " control code is undocumented."

unsupported :: String -> Either String a
unsupported name = Left $ "The " ++ name ++ " control code is unsupported by this binding."

-- | This function takes a 'DWORD' and assumes it is a flagfield. Each bit
--   is masked off and converted into a value. Any failures are silently
--   discarded.
unflag :: DWORD -> [SERVICE_ACCEPT]
unflag f = mapMaybe (hush . fromDWORD . (.&. f)) masks
  where
    masks = take 32 $ iterate (`shiftL` 1) 1

flag :: [SERVICE_ACCEPT] -> DWORD
flag fs = foldl (\flag' f -> flag' .|. toDWORD f) 0 fs
