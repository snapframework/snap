{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Heist.Tests where

import           Control.Error
import           Control.Exception
import qualified Data.ByteString                      as BS
import           Data.List
import qualified Data.Map                             as Map
import           Test.QuickCheck
import           Test.HUnit
import qualified Test.Framework as F
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit       (testCase)

import qualified Heist                                as H
import qualified Heist.Compiled                       as C
import qualified Heist.Interpreted                    as I
import           Snap.Core
import           Snap.Snaplet
import qualified Snap.Test                            as ST
import           Snap.Snaplet.Test
import           Snap.Snaplet.Heist
import qualified Snap.Snaplet.Heist.Compiled          as C
import qualified Snap.Snaplet.Heist.Interpreted       as I
import           Snap.Snaplet.Heist.App
import           SafeCWD

heistTests :: F.Test
heistTests = F.testGroup "Snap.Snaplet.Heist"
             [addTemplatesOK
             ,hasAllTemplates]


------------------------------------------------------------------------------
addTemplatesOK :: F.Test
addTemplatesOK = testCase "Load templates" assertGoodLoadTpl
  where
    assertGoodLoadTpl :: Assertion
    assertGoodLoadTpl = do
      let hdl = with heist $ I.render "foopage"
      res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
      either (assertFailure . show) (ST.assertSuccess) res


------------------------------------------------------------------------------
hasAllTemplates :: F.Test
hasAllTemplates = testCase "Get Heist state" assertHasTemplates
  where
    assertHasTemplates :: Assertion
    assertHasTemplates = do
      let hdl = with heist $  do
            s <- getHeistState
            t <- return $ H.templateNames s
            return $ t
      res <- evalHandler Nothing (ST.get "" Map.empty) hdl appInit
      assertEqual "templateNames, minus foo, bar, and baz"
        (Right []) res

