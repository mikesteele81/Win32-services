module Main where

import Control.Concurrent.MVar
import System.Win32.SystemServices.Services

main = startServiceCtrlDispatcher "Test" $ \name _ -> do
    mStop <- newEmptyMVar
    hStatus <- registerServiceCtrlHandlerEx name $ handler mStop
    setServiceStatus hStatus running
    takeMVar mStop
    setServiceStatus hStatus stopped

handler mStop hStatus STOP = do
    setServiceStatus hStatus stopPending
    putMVar mStop ()
    return True
handler _ _ INTERROGATE = return True
handler _ _ _           = return False

stopped = SERVICE_STATUS WIN32_OWN_PROCESS STOPPED [] nO_ERROR 0 0 0
stopPending = stopped { currentState = STOP_PENDING
                      , waitHint = 3000 }
running = stopped { currentState = RUNNING
                  , controlsAccepted = [ACCEPT_STOP] }