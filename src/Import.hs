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

import System.Win32.Types as X

-- | Suppress the 'Left' value of an 'Either'
-- taken from the errors package
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
