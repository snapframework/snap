{-|

This module provides convenience exports of the modules most commonly used
when developing with the Snap Framework.  For documentation about Snaplets,
see "Snap.Snaplet".  For the core web server API, see "Snap.Core".

-}

module Snap
  ( module Control.Applicative
  , module Control.Monad.State
  , module Data.Lens.Common
  , module Data.Lens.Template
  , module Snap.Core
  , module Snap.Http.Server
  , module Snap.Snaplet
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Lens.Common
import Data.Lens.Template
import Snap.Core
import Snap.Http.Server
import Snap.Snaplet

