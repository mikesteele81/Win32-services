{-# LANGUAGE CPP #-}

module Import
  ( module X
  , hush
  ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative as X
#endif
import Foreign as X

import System.Win32.Error as X
import System.Win32.Error.Foreign as X
import System.Win32.Types as X
    hiding ( ErrCode, failIfNull, failWith, failUnlessSuccess
           , failIfFalse_, failIf, errorWin)

-- | Suppress the 'Left' value of an 'Either'
-- taken from the errors package
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
