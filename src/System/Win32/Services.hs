module System.Win32.Services
    ( HandlerFunction
    , ServiceMainFunction
    , SERVICE_ACCEPT (..)
    , SERVICE_CONTROL (..)
    , SERVICE_STATE (..)
    , SERVICE_STATUS (..)
    , SERVICE_TYPE (..)
    , nO_ERROR
    , eRROR_SERVICE_SPECIFIC_ERROR
    , queryServiceStatus
    , setServiceStatus
    , startServiceCtrlDispatcher
    ) where

import Control.Exception
import Control.Monad.Fix

import Import
import System.Win32.Services.Raw
import System.Win32.Services.Accept
import System.Win32.Services.Control
import qualified System.Win32.Services.Control as SC
import System.Win32.Services.State
import System.Win32.Services.Status
import System.Win32.Services.TableEntry
import System.Win32.Services.Type
import System.Win32.Services.Types

-- | A handler function is registered with the service dispatcher thread
--   from a 'ServiceMainFunction'. The first argument is a 'HANDLE' returned
--   from calling 'registerServiceCtrlHandler'. The second argument represents
--   the command this service has been directed to perform.
type HandlerFunction = HANDLE -> SERVICE_CONTROL -> IO Bool

-- | The service dispatcher thread will call each function of this type that
--   you provide. The first argument will be the name of the service. Any
--   additional command-line parameters will appear in the second argument.
--
--   Each of these functions should call 'registerServiceCtrlHandler' to
--   register a function to handle incoming commands. It should then set
--   the service's status to 'START_PENDING', and specify that no controls
--   will be accepted. At this point the function may perform any other
--   initialization steps before setting the service's status to
--   'RUNNING'. All of this should take no more than 100ms.
type ServiceMainFunction = String -> [String] -> HANDLE -> IO ()

-- |Retrieves the current status of the specified service.
queryServiceStatus :: HANDLE
    -- ^ MSDN documentation: A handle to the service. This handle is returned
    -- by the OpenService or the CreateService function, and it must have the
    -- SERVICE_QUERY_STATUS access right. For more information, see Service
    -- Security and Access Rights.
    -> IO SERVICE_STATUS
    -- ^ This function will raise an exception if the Win32 call returned an
    -- error condition.
queryServiceStatus h = alloca $ \pStatus -> do
    failIfFalse_ (unwords ["QueryServiceStatus"])
        $ c_QueryServiceStatus h pStatus
    peek pStatus

-- | Register an handler function to be called whenever the operating system
-- receives service control messages.
registerServiceCtrlHandlerEx :: String
    -- ^ The name of the service. According to MSDN documentation this
    -- argument is unused in WIN32_OWN_PROCESS type services, which is the
    -- only type supported by this binding. Even so, it is recommended
    -- that the name of the service be used.
    --
    -- MSDN description: The name of the service run by the calling thread.
    -- This is the service name that the service control program specified in
    -- the CreateService function when creating the service.
    -> HandlerFunction
    -- ^ A Handler function to be called in response to service control
    -- messages. Behind the scenes this is translated into a "HandlerEx" type
    -- handler.
    -> IO (HANDLE, LPHANDLER_FUNCTION_EX)
    -- ^ The returned handle may be used in calls to SetServiceStatus. For
    -- convenience Handler functions also receive a handle for the service.
registerServiceCtrlHandlerEx str handler =
    withTString str $ \lptstr ->
    -- use 'ret' instead of (h', _) to avoid divergence.
    mfix $ \ret -> do
    fpHandler <- handlerToFunPtr $ toHandlerEx (fst ret) handler
    h <- failIfNull (unwords ["RegisterServiceCtrlHandlerEx", str])
        $ c_RegisterServiceCtrlHandlerEx lptstr fpHandler nullPtr
    return (h, fpHandler)

-- |Updates the service control manager's status information for the calling
-- service.
setServiceStatus :: HANDLE
    -- ^ MSDN documentation: A handle to the status information structure for
    -- the current service. This handle is returned by the
    -- RegisterServiceCtrlHandlerEx function.
    -> SERVICE_STATUS
    -- ^ MSDN documentation: A pointer to the SERVICE_STATUS structure the
    -- contains the latest status information for the calling service.
    -> IO ()
    -- ^ This function will raise an exception if the Win32 call returned an
    -- error condition.
setServiceStatus h status =
    with status $ \pStatus -> do
    failIfFalse_ (unwords ["SetServiceStatus", show h, show status])
        $ c_SetServiceStatus h pStatus

-- |Register a callback function to initialize the service, which will be
-- called by the operating system immediately. startServiceCtrlDispatcher
-- will block until the provided callback function returns.
--
-- MSDN documentation: Connects the main thread of a service process to the
-- service control manager, which causes the thread to be the service control
-- dispatcher thread for the calling process.
startServiceCtrlDispatcher :: String
    -- ^ The name of the service. According to MSDN documentation this
    -- argument is unused in WIN32_OWN_PROCESS type services, which is the
    -- only type supported by this binding. Even so, it is recommended
    -- that the name of the service be used.
    --
    -- MSDN description: The name of the service run by the calling thread.
    -- This is the service name that the service control program specified in
    -- the CreateService function when creating the service.
    -> DWORD
    -- ^
    -- [@waitHint@] The estimated time required for a pending start, stop,
    -- pause, or continue operation, in milliseconds.
    -> HandlerFunction
    -> ServiceMainFunction
    -- ^ This is a callback function that will be called by the operating
    -- system whenever the service is started. It should perform service
    -- initialization including the registration of a handler function.
    -- MSDN documentation gives conflicting advice as to whether this function
    -- should return before the service has entered the stopped state.
    -- In the official example the service main function blocks until the
    -- service is ready to stop.
    -> IO ()
    -- ^ An exception will be raised if the underlying Win32 call returns an
    -- error condition.
startServiceCtrlDispatcher name wh handler main =
    withTString name $ \lptstr ->
    bracket (toSMF main handler wh >>= smfToFunPtr) freeHaskellFunPtr $ \fpMain ->
    withArray [SERVICE_TABLE_ENTRY lptstr fpMain, nullSTE] $ \pSTE ->
    failIfFalse_ (unwords ["StartServiceCtrlDispatcher", name]) $ do
    c_StartServiceCtrlDispatcher pSTE

toSMF :: ServiceMainFunction -> HandlerFunction -> DWORD -> IO SERVICE_MAIN_FUNCTION
toSMF f handler wh = return $ \len pLPTSTR -> do
    lptstrx <- peekArray (fromIntegral len) pLPTSTR
    args <- mapM peekTString lptstrx
    -- MSDN guarantees args will have at least 1 member.
    let name = head args
    (h, fpHandler) <- registerServiceCtrlHandlerEx name handler
    setServiceStatus h $ SERVICE_STATUS WIN32_OWN_PROCESS START_PENDING [] nO_ERROR 0 0 wh
    f name (tail args) h
    freeHaskellFunPtr fpHandler

-- This was originally written with older style handle functions in mind.
-- I'm now using HandlerEx style functions, and need to add support for
-- the extra parameters here.
toHandlerEx :: HANDLE -> HandlerFunction -> HANDLER_FUNCTION_EX
toHandlerEx h f = \dwControl _ _ _ ->
    case SC.fromDWORD dwControl of
      Right control -> do
          handled <- f h control
          case control of
            INTERROGATE -> return nO_ERROR
            -- If we ever support extended control codes this will have to
            -- change. see "Dev Center - Desktop > Docs > Desktop app
            -- development documentation > System Services > Services >
            -- Service Reference > Service Functions > HandlerEx".
            _ -> return $ if handled then nO_ERROR
                                     else eRROR_CALL_NOT_IMPLEMENTED
      Left _ -> return eRROR_CALL_NOT_IMPLEMENTED
