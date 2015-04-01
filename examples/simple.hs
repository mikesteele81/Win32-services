module Main where

import Control.Concurrent.MVar
import System.Win32.Services

main = do
    mStop <- newEmptyMVar
    startServiceCtrlDispatcher "Test" 3000 (handler mStop) $ \_ _ h -> do
        setServiceStatus h running
        takeMVar mStop
        setServiceStatus h stopped

handler mStop hStatus Stop = do
    setServiceStatus hStatus stopPending
    putMVar mStop ()
    return True
handler _ _ Interrogate = return True
handler _ _ _           = return False

running = ServiceStatus Win32OwnProcess Running [AcceptStop] nO_ERROR 0 0 0
stopped = ServiceStatus Win32OwnProcess Stopped [] nO_ERROR 0 0 0
stopPending = ServiceStatus Win32OwnProcess StopPending [AcceptStop] nO_ERROR 0 0 0
