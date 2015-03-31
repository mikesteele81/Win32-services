module System.Win32.SystemServices.Services.SERVICE_STATUS where

-- These two imports are here to preserve existing behavior now that
-- the dependency on "errors" has been dropped.
import System.Exit
import System.IO

import Import
import System.Win32.SystemServices.Services.SERVICE_ACCEPT
import System.Win32.SystemServices.Services.SERVICE_STATE
import System.Win32.SystemServices.Services.SERVICE_TYPE

-- | Contains status information for a service.
data SERVICE_STATUS = SERVICE_STATUS
    { serviceType             :: SERVICE_TYPE
    -- ^ The type of service. This binding only supports the WIN32_OWN_PROCESS
    -- type.
    , currentState            :: SERVICE_STATE
    -- ^ The current state of the service.
    , controlsAccepted        :: [SERVICE_ACCEPT]
    -- ^ See 'SERVICE_ACCEPT' for details on this field.
    , win32ExitCode           :: DWORD
    -- ^ The error code the service uses to report an error that occurs when
    --   it is starting or stopping. To return an error code specific to the
    --   service, the service must set this value to
    --   'eRROR_SERVICE_SPECIFIC_ERROR' to indicate that the
    --   'serviceSpecificExitCode' member contains the error code. The service
    --   should set this value to 'nO_ERROR' when it is running and on normal
    --   termination.
    , serviceSpecificExitCode :: DWORD
    -- ^ A service-specific error code that the service returns when an error
    -- occurs while the service is starting or stopping. This value is
    -- ignored unless the 'win32ExitCode' member is set to
    -- 'eRROR_SERVICE_SPECIFIC_ERROR'.
    --
    -- This binding does not support service-specific error codes.
    , checkPoint              :: DWORD
    -- ^ The check-point value the service increments periodically to report
    --   its progress during a lengthy start, stop, pause, or continue
    --   operation. For example, the service should increment this value as it
    --   completes each step of its initialization when it is starting up. The
    --   user interface program that invoked the operation on the service uses
    --   this value to track the progress of the service during a lengthy
    --   operation. This value is not valid and should be zero when the
    --   service does not have a start, stop, pause, or continue operation
    --   pending.
    , waitHint                :: DWORD
    -- ^ The estimated time required for a pending start, stop, pause, or
    --   continue operation, in milliseconds. Before the specified amount of
    --   time has elapsed, the service should make its next call to the
    --   SetServiceStatus function with either an incremented dwCheckPoint
    --   value or a change in 'currentState'. If the amount of time specified
    --   by 'waitHint' passes, and 'checkPoint' has not been incremented or
    --   'currentState' has not changed, the service control manager or
    --   service control program can assume that an error has occurred and the
    --   service should be stopped. However, if the service shares a process
    --   with other services, the service control manager cannot terminate the
    --   service application because it would have to terminate the other
    --   services sharing the process as well.
    } deriving (Show)

instance Storable SERVICE_STATUS where
  sizeOf _ = 28
  alignment _ = 4
  peek ptr = do
      -- This block is not ideal. It is here to preserve backwards
      -- compatibility with former behavior, and will be replaced in a future
      -- version. We used to wrap peekServiceType and peekServiceState in
      -- calls to runScript from the "errors" package. This results in a
      -- line being printed to stderr and process termination on a left value.
      -- Service applications do not have stderr.
      est <- peekServiceType (pST ptr)
      eca <- peekServiceState (pCS ptr)
      case (,) <$> est <*> eca of
        Left e -> do
          -- runScript would call this on error.
          hPutStrLn stderr e
          exitFailure
        Right (st, ca) -> SERVICE_STATUS st ca
          <$> (peekServiceAccept . pCA) ptr
          <*> (peek . pEC) ptr
          <*> (peek . pSSEC) ptr
          <*> (peek . pCP) ptr
          <*> (peek . pWH) ptr
  poke ptr (SERVICE_STATUS st cs ca ec ssec cp wh) = do
    pokeServiceType (pST ptr) st
    pokeServiceState (pCS ptr) cs
    pokeServiceAccept (pCA ptr) ca
    poke (pEC ptr) ec
    poke (pSSEC ptr) ssec
    poke (pCP ptr) cp
    poke (pWH ptr) wh

pST, pCS, pCA, pEC, pSSEC, pCP, pWH
    :: Ptr SERVICE_STATUS -> Ptr DWORD

pST   =                  castPtr
pCS   = (`plusPtr` 4)  . castPtr
pCA   = (`plusPtr` 8)  . castPtr
pEC   = (`plusPtr` 12) . castPtr
pSSEC = (`plusPtr` 16) . castPtr
pCP   = (`plusPtr` 20) . castPtr
pWH   = (`plusPtr` 24) . castPtr
