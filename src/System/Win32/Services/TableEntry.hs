module System.Win32.Services.TableEntry where

import Import

type SERVICE_MAIN_FUNCTION = DWORD -> Ptr LPTSTR -> IO ()

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
