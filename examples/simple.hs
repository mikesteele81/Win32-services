module Main where

import Control.Concurrent.MVar
import System.Win32.SystemServices.Services

main = do
    mStop <- newEmptyMVar
    startServiceCtrlDispatcher "Test" 3000 (handler mStop) $ \_ _ h -> do
        setServiceStatus h running
        takeMVar mStop
        setServiceStatus h stopped

handler mStop hStatus STOP = do
    setServiceStatus hStatus stopPending
    putMVar mStop ()
    return True
handler _ _ INTERROGATE = return True
handler _ _ _           = return False

running = SERVICE_STATUS WIN32_OWN_PROCESS RUNNING [ACCEPT_STOP] nO_ERROR 0 0 0
stopped = SERVICE_STATUS WIN32_OWN_PROCESS STOPPED [] nO_ERROR 0 0 0
stopPending = SERVICE_STATUS WIN32_OWN_PROCESS STOP_PENDING [ACCEPT_STOP] nO_ERROR 0 0 0
