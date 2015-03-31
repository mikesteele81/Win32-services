module System.Win32.Services.State
    ( ServiceState (..)
    , nO_ERROR
    , eRROR_CALL_NOT_IMPLEMENTED
    , eRROR_SERVICE_SPECIFIC_ERROR
    , peekServiceState
    , pokeServiceState
    ) where

import Text.Printf

import Import

nO_ERROR :: ErrCode
nO_ERROR = 0

eRROR_CALL_NOT_IMPLEMENTED :: ErrCode
eRROR_CALL_NOT_IMPLEMENTED = 0x78

eRROR_SERVICE_SPECIFIC_ERROR :: ErrCode
eRROR_SERVICE_SPECIFIC_ERROR = 0x42a

-- | The current state of a service.
data ServiceState = ContinuePending | PausePending | Paused | Running
    | StartPending | StopPending | Stopped
    deriving (Eq, Show)

fromDWORD :: DWORD -> Either String ServiceState
fromDWORD 5 = Right ContinuePending
fromDWORD 6 = Right PausePending
fromDWORD 7 = Right Paused
fromDWORD 4 = Right Running
fromDWORD 2 = Right StartPending
fromDWORD 3 = Right StopPending
fromDWORD 1 = Right Stopped
fromDWORD x = Left $ "Unable to interpret " ++ printf "%x" x
                  ++ " as a SERVICE_STATE."

toDWORD :: ServiceState -> DWORD
toDWORD Stopped = 0x00000001
toDWORD StartPending = 0x00000002
toDWORD StopPending = 0x00000003
toDWORD Running = 0x00000004
toDWORD ContinuePending = 0x00000005
toDWORD PausePending = 0x00000006
toDWORD Paused = 0x00000007

peekServiceState :: Ptr DWORD -> IO (Either String ServiceState)
peekServiceState ptr = fromDWORD <$> peek ptr

pokeServiceState :: Ptr DWORD -> ServiceState -> IO ()
pokeServiceState ptr sc = poke ptr . toDWORD $ sc
