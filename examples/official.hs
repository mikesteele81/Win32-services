module Main where

import Control.Concurrent.MVar
import System.Win32.SystemServices.Services
import System.Win32.Types

main :: IO ()
main = do
    gState <- newMVar (1, SERVICE_STATUS WIN32_OWN_PROCESS
                          START_PENDING [] nO_ERROR 0 0 3000)
    mStop <- newEmptyMVar
    startServiceCtrlDispatcher "Test" 3000 (svcCtrlHandler mStop gState) $ svcMain mStop gState

svcMain mStop gState _ _ h = do
    reportSvcStatus h RUNNING nO_ERROR 0 gState
    takeMVar mStop

reportSvcStatus :: HANDLE -> SERVICE_STATE -> DWORD -> DWORD
    -> MVar (DWORD, SERVICE_STATUS) -> IO ()
reportSvcStatus hStatus state win32ExitCode waitHint mState = do
    modifyMVar_ mState $ \(checkPoint, svcStatus) -> do
        let state' = nextState (checkPoint, svcStatus
             { win32ExitCode = win32ExitCode
             , waitHint      = waitHint
             , currentState  = state })
        setServiceStatus hStatus (snd state')
        return state'

nextState :: (DWORD, SERVICE_STATUS) -> (DWORD, SERVICE_STATUS)
nextState (checkPoint, svcStatus) = case (currentState svcStatus) of 
    START_PENDING -> (checkPoint + 1, svcStatus
        { controlsAccepted = [], checkPoint = checkPoint + 1 })
    RUNNING -> (checkPoint, svcStatus
        { controlsAccepted = [ACCEPT_STOP], checkPoint = 0 })
    STOPPED -> (checkPoint, svcStatus
        { controlsAccepted = [], checkPoint = 0 })
    _ -> (checkPoint + 1, svcStatus
        { controlsAccepted = [], checkPoint = checkPoint + 1 })

svcCtrlHandler :: MVar () -> MVar (DWORD, SERVICE_STATUS)
    -> HandlerFunction
svcCtrlHandler mStop mState hStatus STOP = do
    reportSvcStatus hStatus STOP_PENDING nO_ERROR 3000 mState
    putMVar mStop ()
    return True
svcCtrlHandler _ _ _ INTERROGATE = return True
svcCtrlHandler _ _ _ _  = return False

