module System.Win32.Services.Status where

import qualified System.Win32.Error as E

import Import
import System.Win32.Services.Accept
import System.Win32.Services.State
import System.Win32.Services.Type

-- | Contains status information for a service.
data ServiceStatus = ServiceStatus
    { serviceType             :: ServiceType
    -- ^ The type of service. This binding only supports the 'Win32OwnProcess'
    -- type.
    , currentState            :: ServiceState
    -- ^ The current state of the service.
    , controlsAccepted        :: [ServiceAccept]
    -- ^ See 'ServiceAccept' for details on this field.
    , win32ExitCode           :: E.ErrCode
    -- ^ The error code the service uses to report an error that occurs when
    --   it is starting or stopping. To return an error code specific to the
    --   service, the service must set this value to
    --   'E.ServiceSpecificError' to indicate that the
    --   'serviceSpecificExitCode' member contains the error code. The service
    --   should set this value to 'E.Success' when it is running and on normal
    --   termination.
    , serviceSpecificExitCode :: DWORD
    -- ^ A service-specific error code that the service returns when an error
    -- occurs while the service is starting or stopping. This value is
    -- ignored unless the 'win32ExitCode' member is set to
    -- 'E.ServiceSpecificError'.
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

instance Storable ServiceStatus where
  sizeOf _ = 28
  alignment _ = 4
  peek ptr = ServiceStatus
    <$> (peek . pST) ptr
    <*> (peek . pCS) ptr
    <*> (peekServiceAccept . pCA) ptr
    <*> (peek . pEC) ptr
    <*> (peek . pSSEC) ptr
    <*> (peek . pCP) ptr
    <*> (peek . pWH) ptr
  poke ptr (ServiceStatus st cs ca ec ssec cp wh) = do
    poke (pST ptr) st
    poke (pCS ptr) cs
    pokeServiceAccept (pCA ptr) ca
    poke (pEC ptr) ec
    poke (pSSEC ptr) ssec
    poke (pCP ptr) cp
    poke (pWH ptr) wh

pCA, pSSEC, pCP, pWH :: Ptr ServiceStatus -> Ptr DWORD
pCA   = (`plusPtr` 8)  . castPtr
pSSEC = (`plusPtr` 16) . castPtr
pCP   = (`plusPtr` 20) . castPtr
pWH   = (`plusPtr` 24) . castPtr

pST :: Ptr ServiceStatus -> Ptr ServiceType
{-# INLINE pST #-}
pST = castPtr

pCS :: Ptr ServiceStatus -> Ptr ServiceState
{-# INLINE pCS #-}
pCS = (`plusPtr` 4)  . castPtr

pEC :: Ptr ServiceStatus -> Ptr E.ErrCode
pEC   = (`plusPtr` 12) . castPtr
