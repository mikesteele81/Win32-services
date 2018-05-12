{-# LANGUAGE CPP #-}

module System.Win32.Services.TableEntry where

import Import

type SERVICE_MAIN_FUNCTION = DWORD -> Ptr LPTSTR -> IO ()

data ServiceTableEntry = ServiceTableEntry
    { serviceName :: LPWSTR
    , serviceProc :: FunPtr SERVICE_MAIN_FUNCTION
    }

#if defined(i386_HOST_ARCH)

instance Storable ServiceTableEntry where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = ServiceTableEntry <$> peek pServiceName <*> peek pServiceProc
    where
      pServiceName = castPtr ptr
      pServiceProc = castPtr ptr `plusPtr` 4
  poke ptr ste = do
      poke pServiceName . serviceName $ ste
      poke pServiceProc . serviceProc $ ste
    where
      pServiceName = castPtr ptr
      pServiceProc = castPtr ptr `plusPtr` 4

#elif defined(x86_64_HOST_ARCH)

instance Storable ServiceTableEntry where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = ServiceTableEntry <$> peek pServiceName <*> peek pServiceProc
    where
      pServiceName = castPtr ptr
      pServiceProc = castPtr ptr `plusPtr` 8
  poke ptr ste = do
      poke pServiceName . serviceName $ ste
      poke pServiceProc . serviceProc $ ste
    where
      pServiceName = castPtr ptr
      pServiceProc = castPtr ptr `plusPtr` 8

#else
# error Unsupported architecture
#endif

nullSTE :: ServiceTableEntry
nullSTE = ServiceTableEntry nullPtr nullFunPtr
