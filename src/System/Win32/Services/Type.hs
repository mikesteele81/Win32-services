module System.Win32.Services.Type
    ( ServiceType (..)
    , peekServiceType
    , pokeServiceType
    ) where

import Text.Printf

import Import

-- | Win32 defines many types of services, but this binding only supports
-- Win32OwnProcess.
data ServiceType
    -- | The service is a file system driver.
    = FileSystemDriver
    -- | The service is a device driver.
    | KernelDriver
    -- | The service runs in its own process.
    | Win32OwnProcess
    -- | The service shares a process with other services.
    | Win32ShareProcess
    -- | Do no write your own services of this type. Windows Vista and above
    --   prevent service processes from directly interacting with users.
    --
    --   A 'ServiceInteractiveProcess' is either a 'Win32OwnProcess' or a
    --   'Win32ShareProcess' running in the context of the LocalSystem
    --   account which is allowed to directly interact with users.
    | ServiceInteractiveProcess
    deriving (Show)

toDWORD :: ServiceType -> DWORD
toDWORD FileSystemDriver          = 0x00000002
toDWORD KernelDriver              = 0x00000001
toDWORD Win32OwnProcess           = 0x00000010
toDWORD Win32ShareProcess         = 0x00000020
toDWORD ServiceInteractiveProcess = 0x00000100

fromDWORD :: DWORD -> Either String ServiceType
fromDWORD 0x00000002 = Right FileSystemDriver
fromDWORD 0x00000001 = Right KernelDriver
fromDWORD 0x00000010 = Right Win32OwnProcess
fromDWORD 0x00000020 = Right Win32ShareProcess
fromDWORD 0x00000100 = Right ServiceInteractiveProcess
fromDWORD x = Left $ "Invalid SERVICE_TYPE: " ++ printf "%x" x

peekServiceType :: Ptr DWORD -> IO (Either String ServiceType)
peekServiceType ptr = fromDWORD <$> peek ptr

pokeServiceType :: Ptr DWORD -> ServiceType -> IO ()
pokeServiceType ptr x = poke ptr . toDWORD $ x
