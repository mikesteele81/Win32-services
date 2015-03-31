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

running = ServiceStatus WIN32_OWN_PROCESS Running [ACCEPT_STOP] nO_ERROR 0 0 0
stopped = ServiceStatus WIN32_OWN_PROCESS Stopped [] nO_ERROR 0 0 0
stopPending = ServiceStatus WIN32_OWN_PROCESS StopPending [ACCEPT_STOP] nO_ERROR 0 0 0
