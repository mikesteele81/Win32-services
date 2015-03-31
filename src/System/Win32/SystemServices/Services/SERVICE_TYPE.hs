module System.Win32.SystemServices.Services.SERVICE_TYPE
    ( SERVICE_TYPE (..)
    , peekServiceType
    , pokeServiceType
    ) where

import Text.Printf

import Import

-- | Win32 defines many types of services, but this binding only supports
-- WIN32_OWN_PROCESS.
data SERVICE_TYPE
    -- | The service is a file system driver.
    = FILE_SYSTEM_DRIVER
    -- | The service is a device driver.
    | KERNEL_DRIVER
    -- | The service runs in its own process.
    | WIN32_OWN_PROCESS
    -- | The service shares a process with other services.
    | WIN32_SHARE_PROCESS
    -- | Do no write your own services of this type. Windows Vista and above
    --   prevent service processes from directly interacting with users.
    --
    --   A 'SERVICE_INTERACTIVE_PROCESS' is either a 'WIN32_OWN_PROCESS' or a
    --   'WIN32_SHARE_PROCESS' running in the context of the LocalSystem
    --   account which is allowed to directly interact with users.
    | SERVICE_INTERACTIVE_PROCESS
    deriving (Show)

toDWORD :: SERVICE_TYPE -> DWORD
toDWORD FILE_SYSTEM_DRIVER          = 0x00000002
toDWORD KERNEL_DRIVER               = 0x00000001
toDWORD WIN32_OWN_PROCESS           = 0x00000010
toDWORD WIN32_SHARE_PROCESS         = 0x00000020
toDWORD SERVICE_INTERACTIVE_PROCESS = 0x00000100

fromDWORD :: DWORD -> Either String SERVICE_TYPE
fromDWORD 0x00000002 = Right FILE_SYSTEM_DRIVER
fromDWORD 0x00000001 = Right KERNEL_DRIVER
fromDWORD 0x00000010 = Right WIN32_OWN_PROCESS
fromDWORD 0x00000020 = Right WIN32_SHARE_PROCESS
fromDWORD 0x00000100 = Right SERVICE_INTERACTIVE_PROCESS
fromDWORD x = Left $ "Invalid SERVICE_TYPE: " ++ printf "%x" x

peekServiceType :: Ptr DWORD -> IO (Either String SERVICE_TYPE)
peekServiceType ptr = fromDWORD <$> peek ptr

pokeServiceType :: Ptr DWORD -> SERVICE_TYPE -> IO ()
pokeServiceType ptr x = poke ptr . toDWORD $ x
