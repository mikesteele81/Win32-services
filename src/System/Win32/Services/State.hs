module System.Win32.Services.State
    ( ServiceState (..)
    , nO_ERROR
    , eRROR_CALL_NOT_IMPLEMENTED
    , eRROR_SERVICE_SPECIFIC_ERROR
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

-- |State is stored as a DWORD value in memory.  If an undocument value is
-- encountered during a 'peek', there isn't any reasonable way to respond, so
-- an 'ErrorCall' exception will be thrown.
instance Storable ServiceState where
  sizeOf _ = sizeOf (undefined :: DWORD)
  alignment _ = alignment (undefined :: DWORD)
  peek ptr = marshIn <$> (peek . pDWORD) ptr
  poke ptr state = poke (pDWORD ptr) (marshOut state)

pDWORD :: Ptr ServiceState -> Ptr DWORD
{-# INLINE pDWORD #-}
pDWORD = castPtr

marshIn :: DWORD -> ServiceState
marshIn 5 = ContinuePending
marshIn 6 = PausePending
marshIn 7 = Paused
marshIn 4 = Running
marshIn 2 = StartPending
marshIn 3 = StopPending
marshIn 1 = Stopped
marshIn x = error $ printf "%x is not a valid SERVICE_STATE value." x

marshOut :: ServiceState -> DWORD
marshOut Stopped = 0x00000001
marshOut StartPending = 0x00000002
marshOut StopPending = 0x00000003
marshOut Running = 0x00000004
marshOut ContinuePending = 0x00000005
marshOut PausePending = 0x00000006
marshOut Paused = 0x00000007
