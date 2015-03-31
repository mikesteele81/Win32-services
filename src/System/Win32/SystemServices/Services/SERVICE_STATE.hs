module System.Win32.SystemServices.Services.SERVICE_STATE
    ( SERVICE_STATE (..)
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
data SERVICE_STATE = CONTINUE_PENDING | PAUSE_PENDING | PAUSED | RUNNING
    | START_PENDING | STOP_PENDING | STOPPED
    deriving (Eq, Show)

fromDWORD :: DWORD -> Either String SERVICE_STATE
fromDWORD 5 = Right CONTINUE_PENDING
fromDWORD 6 = Right PAUSE_PENDING
fromDWORD 7 = Right PAUSED
fromDWORD 4 = Right RUNNING
fromDWORD 2 = Right START_PENDING
fromDWORD 3 = Right STOP_PENDING
fromDWORD 1 = Right STOPPED
fromDWORD x = Left $ "Unable to interpret " ++ printf "%x" x
                  ++ " as a SERVICE_STATE."

toDWORD :: SERVICE_STATE -> DWORD
toDWORD STOPPED          = 0x00000001
toDWORD START_PENDING    = 0x00000002
toDWORD STOP_PENDING     = 0x00000003
toDWORD RUNNING          = 0x00000004
toDWORD CONTINUE_PENDING = 0x00000005
toDWORD PAUSE_PENDING    = 0x00000006
toDWORD PAUSED           = 0x00000007

peekServiceState :: Ptr DWORD -> IO (Either String SERVICE_STATE)
peekServiceState ptr = fromDWORD <$> peek ptr

pokeServiceState :: Ptr DWORD -> SERVICE_STATE -> IO ()
pokeServiceState ptr sc = poke ptr . toDWORD $ sc
