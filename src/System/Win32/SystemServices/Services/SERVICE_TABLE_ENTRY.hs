module System.Win32.SystemServices.Services.SERVICE_TABLE_ENTRY where

import Import
import System.Win32.SystemServices.Services.Types

data SERVICE_TABLE_ENTRY = SERVICE_TABLE_ENTRY
    { serviceName :: LPWSTR
    , serviceProc :: FunPtr SERVICE_MAIN_FUNCTION
    }

instance Storable SERVICE_TABLE_ENTRY where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = SERVICE_TABLE_ENTRY <$> peek pServiceName <*> peek pServiceProc
    where
      pServiceName = castPtr ptr
      pServiceProc = castPtr ptr `plusPtr` 4
  poke ptr ste = do
      poke pServiceName . serviceName $ ste
      poke pServiceProc . serviceProc $ ste
    where
      pServiceName = castPtr ptr
      pServiceProc = castPtr ptr `plusPtr` 4

nullSTE :: SERVICE_TABLE_ENTRY
nullSTE = SERVICE_TABLE_ENTRY nullPtr nullFunPtr
