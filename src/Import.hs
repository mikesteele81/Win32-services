{-# LANGUAGE CPP #-}

module Import
  ( module X
  ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative as X
#endif
import Foreign as X

import System.Win32.Types as X
