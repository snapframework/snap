{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Blackbox.App where


------------------------------------------------------------------------------
import Prelude hiding (lookup)

------------------------------------------------------------------------------
import Control.Applicative
import Control.Lens
import Control.Monad.Trans
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Configurator
import Snap.Core
import Snap.Util.FileServe

------------------------------------------------------------------------------
import Data.Map.Syntax ((##))
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Snap.Snaplet.HeistNoClass as HNC
import Heist
import Heist.Interpreted

------------------------------------------------------------------------------
import Blackbox.Common
import Blackbox.BarSnaplet
import Blackbox.FooSnaplet
import Blackbox.EmbeddedSnaplet
import Blackbox.Types
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession


                            --------------------
                            --  THE SNAPLET   --
                            --------------------
  
------------------------------------------------------------------------------
