module Main where

import Control.Concurrent.MVar
import System.Win32.SystemServices.Services
import System.Win32.Types

main :: IO ()
main = do
    gState <- newMVar (1, ServiceStatus WIN32_OWN_PROCESS
                          StartPending [] nO_ERROR 0 0 3000)
    mStop <- newEmptyMVar
    startServiceCtrlDispatcher "Test" 3000 (svcCtrlHandler mStop gState) $ svcMain mStop gState

svcMain mStop gState _ _ h = do
    reportSvcStatus h Running nO_ERROR 0 gState
    takeMVar mStop
    reportSvcStatus h Stopped nO_ERROR 0 gState

reportSvcStatus :: HANDLE -> ServiceState -> DWORD -> DWORD
    -> MVar (DWORD, ServiceStatus) -> IO ()
reportSvcStatus hStatus state win32ExitCode waitHint mState = do
    modifyMVar_ mState $ \(checkPoint, svcStatus) -> do
        let state' = nextState (checkPoint, svcStatus
             { win32ExitCode = win32ExitCode
             , waitHint      = waitHint
             , currentState  = state })
        setServiceStatus hStatus (snd state')
        return state'

nextState :: (DWORD, ServiceStatus) -> (DWORD, ServiceStatus)
nextState (checkPoint, svcStatus) = case (currentState svcStatus) of 
    StartPending -> (checkPoint + 1, svcStatus
        { controlsAccepted = [], checkPoint = checkPoint + 1 })
    Running -> (checkPoint, svcStatus
        { controlsAccepted = [ACCEPT_STOP], checkPoint = 0 })
    Stopped -> (checkPoint, svcStatus
        { controlsAccepted = [], checkPoint = 0 })
    _ -> (checkPoint + 1, svcStatus
        { controlsAccepted = [], checkPoint = checkPoint + 1 })

svcCtrlHandler :: MVar () -> MVar (DWORD, ServiceStatus)
    -> HandlerFunction
svcCtrlHandler mStop mState hStatus STOP = do
    reportSvcStatus hStatus StopPending nO_ERROR 3000 mState
    putMVar mStop ()
    return True
svcCtrlHandler _ _ _ INTERROGATE = return True
svcCtrlHandler _ _ _ _  = return False

