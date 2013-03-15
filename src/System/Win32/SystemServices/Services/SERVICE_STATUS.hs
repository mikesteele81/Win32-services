module System.Win32.SystemServices.Services.SERVICE_STATUS where

import Control.Applicative
import Foreign
import System.Win32.Types

import Control.Error

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
  peek ptr = SERVICE_STATUS <$> (runScript . peekServiceType) pST
      <*> (runScript . peekServiceState) pCS <*> peekServiceAccept pCA
      <*> peek pEC <*> peek pSSEC <*> peek pCP <*> peek pWH
    where
      pST = castPtr ptr
      pCS = castPtr ptr `plusPtr` 4
      pCA = castPtr ptr `plusPtr` 8
      pEC = castPtr ptr `plusPtr` 12
      pSSEC = castPtr ptr `plusPtr` 16
      pCP = castPtr ptr `plusPtr` 20
      pWH = castPtr ptr `plusPtr` 24
  poke ptr (SERVICE_STATUS st cs ca ec ssec cp wh) = do
    pokeServiceType (castPtr ptr) st
    pokeServiceState (castPtr ptr `plusPtr` 4) cs
    pokeServiceAccept (castPtr ptr `plusPtr` 8)  ca
    poke (castPtr ptr `plusPtr` 12) ec
    poke (castPtr ptr `plusPtr` 16) ssec
    poke (castPtr ptr `plusPtr` 20) cp
    poke (castPtr ptr `plusPtr` 24) wh

