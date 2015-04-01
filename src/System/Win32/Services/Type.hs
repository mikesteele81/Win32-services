module System.Win32.Services.Type
    ( ServiceType (..)
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

instance Storable ServiceType where
  sizeOf _ = sizeOf (undefined :: DWORD)
  alignment _ = alignment (undefined :: DWORD)
  peek ptr = marshIn <$> (peek . pDWORD) ptr
  poke ptr t = poke (pDWORD ptr) (marshOut t)

pDWORD :: Ptr ServiceType -> Ptr DWORD
{-# INLINE pDWORD #-}
pDWORD = castPtr

marshOut :: ServiceType -> DWORD
marshOut FileSystemDriver          = 0x00000002
marshOut KernelDriver              = 0x00000001
marshOut Win32OwnProcess           = 0x00000010
marshOut Win32ShareProcess         = 0x00000020
marshOut ServiceInteractiveProcess = 0x00000100

marshIn :: DWORD -> ServiceType
marshIn 0x00000002 = FileSystemDriver
marshIn 0x00000001 = KernelDriver
marshIn 0x00000010 = Win32OwnProcess
marshIn 0x00000020 = Win32ShareProcess
marshIn 0x00000100 = ServiceInteractiveProcess
marshIn x = error $ printf "Invalid SERVICE_TYPE: %x" x
