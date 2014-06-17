{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Heist.Tests where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString                      as BS
import           Data.List
import qualified Data.Set                             as Set
import qualified Data.Map                             as Map
import qualified Data.Text                            as T
import           Test.QuickCheck
import           Test.HUnit
import qualified Test.Framework as F
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit       (testCase)

import           Data.Map.Syntax                      ((##))
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
import qualified Text.XmlHtml                         as XML

heistTests :: F.Test
heistTests = F.testGroup "Snap.Snaplet.Heist"
             [testCase "Load templates" addTemplatesOK
             ,testCase "Get Heist state" assertHasTemplates
             ,testCase "Handler with heist state" accessibleHeistState
             ,testCase "Render a small template" simpleRender]


------------------------------------------------------------------------------
addTemplatesOK :: Assertion
addTemplatesOK = do
  let hdl = with heist $ I.render "foopage"
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) (ST.assertSuccess) res


------------------------------------------------------------------------------
assertHasTemplates :: Assertion
assertHasTemplates = do
  let hdl = with heist $  do
        s <- getHeistState
        t <- return $ H.templateNames s
        return $ Set.fromList (map head t)
  res <- evalHandler Nothing (ST.get "" Map.empty) hdl appInit
  assertBool "templateNames include foopage, barpage, bazpage" $ 
    (Right (Set.fromList [])) ==
    (Set.difference
     (Set.fromList ["foopage","barpage","bazpage"])
     <$> res)


------------------------------------------------------------------------------
accessibleHeistState :: Assertion
accessibleHeistState = do
  let hdl = with heist . withHeistState $
        I.lookupSplice "thisSpliceDoesntExist"
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) (ST.assertSuccess) res


------------------------------------------------------------------------------
simpleRender :: Assertion
simpleRender = do
  let hdl = with heist $ gRender "foopage"
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res

simpleRenderAsOK :: Assertion
simpleRenderAsOK = do
  let hdl = with heist $ gRenderAs "text/html" "foopage"
  res <- 
