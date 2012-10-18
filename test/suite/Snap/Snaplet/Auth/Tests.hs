{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Snap.Snaplet.Auth.Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Lens.Template
import           Data.List
import           Data.Text
import           Prelude hiding (catch, (.))
import           System.Directory
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
------------------------------------------------------------------------------
import qualified Snap.Snaplet.Auth.Handlers.Tests


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Auth"
    [ Snap.Snaplet.Auth.Handlers.Tests.tests
    ]

