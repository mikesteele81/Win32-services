-- We refer to otherwise unused modules in documentation.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module System.Win32.Services.Accept
    ( ServiceAccept (..)
    , pokeServiceAccept
    , peekServiceAccept
    ) where

import Data.Bits
import Data.Maybe
import Text.Printf

import Import

-- Imported for haddocks
import qualified System.Win32.Services.Control as C

-- | The control codes the service accepts and processes in its handler
--   function (See 'HandlerFunction'). By default, all services accept the
--   'C.Interrogate' value. To accept the 'DEVICEEVENT' value, the service must
--   register to receive device events by using the
--   'registerDeviceNotification' function.
data ServiceAccept
    -- | The service is a network component that can accept changes in its
    --   binding without being stopped and restarted. This control code allows
    --   the service to receive 'C.NetBindAdd', 'C.NetBindRemove',
    --   'C.NetBindEnable', and 'C.NetBindDisable' notifications.
    = AcceptNetBindChange
    -- | The service can reread its startup parameters without being stopped
    --   and restarted. This control code allows the service to receive
    --   'C.ParamChange' notifications.
    | AcceptParamChange
    -- | The service can be paused and continued. This control code allows the
    --   service to receive 'C.Pause' and 'C.Continue' notifications.
    | AcceptPauseContinue
    -- | MSDN documentation says that this function is not supported on
    --   Windows Server 2003 or Windows XP/2000. The support status on other
    --   versions is unknown to me.
    --
    --   The service can perform preshutdown tasks. This control code enables
    --   the service to receive 'C.Preshutdown' notifications.
    --   Note that only the system can send it.
    | AcceptPreshutdown
    -- | The service is notified when system shutdown occurs. This control
    --   code allows the service to receive 'C.Shutdown' notifications. Note
    --   that only the system can send it.
    | AcceptShutdown
    -- | The service can be stopped. This control code allows the service to
    --   receive 'C.Stop' notifications.
    | AcceptStop
    deriving (Show)

peekServiceAccept :: Ptr DWORD -> IO [ServiceAccept]
peekServiceAccept ptr = unflag <$> peek ptr

pokeServiceAccept :: Ptr DWORD -> [ServiceAccept] -> IO ()
pokeServiceAccept ptr sas = poke ptr . flag $ sas

-- | Marshal a ServiceAccept "out" to be used in C-land
marshOut :: ServiceAccept -> DWORD
marshOut AcceptNetBindChange = 0x00000010
marshOut AcceptParamChange   = 0x00000008
marshOut AcceptPauseContinue = 0x00000002
marshOut AcceptPreshutdown   = 0x00000100
marshOut AcceptShutdown      = 0x00000004
marshOut AcceptStop          = 0x00000001

-- | Marshall a DWORD "in" to be used in Haskell-land as a ServiceAccept
marshIn :: DWORD -> Either String ServiceAccept
marshIn 0x00000010 = Right AcceptNetBindChange
marshIn 0x00000008 = Right AcceptParamChange
marshIn 0x00000002 = Right AcceptPauseContinue
marshIn 0x00000100 = Right AcceptPreshutdown
marshIn 0x00000004 = Right AcceptShutdown
marshIn 0x00000001 = Right AcceptStop
marshIn 0x00000020 = unsupported "SERVICE_ACCEPT_HARDWAREPROFILECHANGE"
marshIn 0x00000040 = unsupported "SERVICE_ACCEPT_POWEREVENT"
marshIn 0x00000080 = unsupported "SERVICE_ACCEPT_SESSIONCHANGE"
marshIn 0x00000200 = unsupported "SERVICE_ACCEPT_TIMECHANGE"
marshIn 0x00000400 = unsupported "SERVICE_ACCEPT_TRIGGEREVENT"
marshIn 0x00000800 = unsupported "SERVICE_ACCEPT_USERMODEREBOOT"
marshIn x = Left $ "The " ++ printf "%x" x ++ " control code is undocumented."

unsupported :: String -> Either String a
unsupported name = Left $ "The " ++ name ++ " control code is unsupported by this binding."

-- | This function takes a 'DWORD' and assumes it is a flagfield. Each bit
--   is masked off and converted into a value. Any failures are silently
--   discarded.
unflag :: DWORD -> [ServiceAccept]
unflag f = mapMaybe (hush . marshIn . (.&. f)) masks
  where
    masks = take 32 $ iterate (`shiftL` 1) 1

flag :: [ServiceAccept] -> DWORD
flag fs = foldl (\flag' f -> flag' .|. marshOut f) 0 fs
