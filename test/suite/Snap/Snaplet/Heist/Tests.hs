{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Heist.Tests where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                  (join)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.ByteString.Char8          as BSC
import           Data.List                      (isInfixOf)
import qualified Data.Set                       as Set
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import           Test.HUnit                     (Assertion, assertBool,
                                                 assertFailure)
import qualified Test.Framework                 as F
import           Test.Framework.Providers.HUnit (testCase)
------------------------------------------------------------------------------
import           Data.Map.Syntax                ((##))
import qualified Heist                          as H
import qualified Heist.Interpreted              as I
import           Snap.Snaplet                   (with)
import qualified Snap.Test                      as ST
import           Snap.TestCommon                (expectException)
import           Snap.Snaplet.Test              (evalHandler, runHandler)
import qualified Snap.Snaplet.Heist             as HS
import qualified Snap.Snaplet.Heist.Compiled    as C
import qualified Snap.Snaplet.Heist.Interpreted as I
import           Snap.Snaplet.Test.Common.App   (appInit, appInit', heist)
import qualified Text.XmlHtml                   as XML

heistTests :: F.Test
heistTests = F.testGroup "Snap.Snaplet.Heist"
             [testCase "Load templates" addTemplatesOK
             ,testCase "Get Heist state" assertHasTemplates
             ,testCase "Handler with heist state" accessibleHeistState
             ,testCase "gRender a template" gSimpleRender
--             ,testCase "gRender another template" gSimpleRenderAnother -- TODO investigate
             ,testCase "cRender a template" (simpleRender False)
             ,testCase "Render a template"  (simpleRender True)
             ,testCase "gRenderAs a small template" gSimpleRenderAs
             ,testCase "cRenderAs a template" (simpleRenderAs False)
             ,testCase "renderAs a template"  (simpleRenderAs True)
             ,testCase "gServe existing template" gSimpleHeistServeOK
             ,testCase "cServe templates" (simpleHeistServeOK False)
             ,testCase "serve templates" (simpleHeistServeOK True)
             ,testCase "gHeistServe underscore template" gSimpleHeistServeUnd
             ,testCase "gHeistServe missing template" gSimpleHeistServeMissing
             ,testCase "gHeistServeSingle template" gSimpleHeistServeSingle
             ,testCase "cHeistServeSingle template"
              (simpleHeistServeSingle False)
             ,testCase "heistServeSingle template"
              (simpleHeistServeSingle True)
             ,testCase "gHeistServeSingle underscored template"
              gSimpleHeistServeSingleUnd
             ,testCase "gHeistServeSingle missing template"
              gSimpleHeistServeSingleMissing
             ,testCase "Choose compiled mode" chooseCompiled
             ,testCase "Choose interpreted mode" chooseInterpreted
             ,testCase "Render with splices" fooRenderWith
             ,testCase "Recognize withSplices" seeLocalSplices
             ,testCase "Recognize heistLocal" seeLocalState
             ,testCase "cRender with compiled module" compiledModuleRender
             ,testCase "cRenderAs compiled module" compiledModuleRenderAs
             ,testCase "cHeistServe a template" compiledModuleServe
             ,testCase "cHeistServeSingle a template" compiledModuleServeOne
             ]


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
        s  <- HS.getHeistState
        t  <- return $ H.templateNames s
        sp <- return $ H.spliceNames s
        sc <- return $ H.compiledSpliceNames s
        liftIO $ putStrLn $ "Templates " ++ unwords (map show t)
        liftIO $ putStrLn $ "Splices: " ++ unwords (map show sp)
        liftIO $ putStrLn $ "Compiled splices: " ++ unwords (map show sc)
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
  let hdl = with heist . HS.withHeistState $
        I.lookupSplice "thisSpliceDoesntExist"
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) (ST.assertSuccess) res


------------------------------------------------------------------------------
gSimpleRender :: Assertion
gSimpleRender = do
  let hdl = with heist $ HS.gRender "foopage"
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res

gSimpleRenderAnother :: Assertion
gSimpleRenderAnother = do
  let hdl = with heist $ HS.gRender "bazpage"
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res

------------------------------------------------------------------------------
simpleRender :: Bool -> Assertion
simpleRender interp = do
  let hdl = with heist $
            HS.chooseMode (HS.cRender "foopage") (HS.render "foopage")
  res <- runHandler Nothing (ST.get "" Map.empty) hdl
         (appInit' interp False)
  either (assertFailure . show) ST.assertSuccess res


------------------------------------------------------------------------------
gSimpleRenderAs :: Assertion
gSimpleRenderAs = do
  let hdl = with heist $ HS.gRenderAs "audio/ogg" "foopage"
      defReq = ST.get "" Map.empty
      rs = either (return . T.unpack)
           (\r -> (BSC.unpack <$> ST.responseToString r))
  resStr <- join $ rs <$> runHandler Nothing defReq hdl appInit
  assertBool "gRenderAs should set content to audio/ogg" $
    ("audio/ogg" `isInfixOf` resStr)


------------------------------------------------------------------------------
simpleRenderAs :: Bool -> Assertion
simpleRenderAs interp = do
  let hdl = with heist $ HS.chooseMode
            (HS.cRenderAs "audio/ogg" "foopage")
            (HS.renderAs  "audio/ogg" "foopage")
      defReq = ST.get "" Map.empty
      rs  = either (return . T.unpack)
            (\r -> (BSC.unpack <$> ST.responseToString r))

  resStr <- join $ rs <$> runHandler Nothing defReq hdl
                          (appInit' interp False)
  assertBool "renderAs should set content to audio/ogg" $
        ("audio/ogg" `isInfixOf` resStr)


------------------------------------------------------------------------------
gSimpleHeistServeOK :: Assertion
gSimpleHeistServeOK = do
  let hdl = with heist HS.gHeistServe
  res <- runHandler Nothing (ST.get "index" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res


------------------------------------------------------------------------------
simpleHeistServeOK :: Bool -> Assertion
simpleHeistServeOK interp = do
  let hdl = with heist $ HS.chooseMode HS.cHeistServe HS.heistServe
  res <- runHandler Nothing (ST.get "foopage" Map.empty) hdl
         (appInit' interp False)
  either (assertFailure . show) ST.assertSuccess res

------------------------------------------------------------------------------
gSimpleHeistServeUnd :: Assertion
gSimpleHeistServeUnd = do
  let hdl = with heist HS.gHeistServe
  res <- runHandler Nothing (ST.get "_foopage" Map.empty) hdl appInit
  either (assertFailure . show) ST.assert404 res


------------------------------------------------------------------------------
gSimpleHeistServeMissing :: Assertion
gSimpleHeistServeMissing = do
  let hdl = with heist HS.gHeistServe
  res <- runHandler Nothing (ST.get "nonexisting" Map.empty) hdl appInit
  either (assertFailure . show) ST.assert404 res


simpleHeistServeSingle :: Bool -> Assertion
simpleHeistServeSingle interp = do
  let hdl = with heist $ HS.chooseMode
            (HS.cHeistServeSingle "foopage")
            (HS.heistServeSingle  "foopage")
  res <- runHandler Nothing (ST.get "foopage" Map.empty) hdl
         (appInit' interp False)
  either (assertFailure . show) ST.assertSuccess res

------------------------------------------------------------------------------
-- Serves foopage, despite request for nonexistent
gSimpleHeistServeSingle :: Assertion
gSimpleHeistServeSingle = do
  let hdl = with heist $ HS.gHeistServeSingle "foopage"
  res <- runHandler Nothing (ST.get "nonexistent" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res


------------------------------------------------------------------------------
-- serveSingle does not filter out underscored templates
gSimpleHeistServeSingleUnd :: Assertion
gSimpleHeistServeSingleUnd = do
  let hdl = with heist $ I.heistServeSingle "_foopage"
  res <- runHandler Nothing (ST.get "_foopage" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res

------------------------------------------------------------------------------
gSimpleHeistServeSingleMissing :: Assertion
gSimpleHeistServeSingleMissing = do
  let hdl = with heist $ HS.gHeistServeSingle "nonexistent"
  expectException
    "gHeistServeSingle failed to throw when serving nonexistent template"
    (runHandler Nothing (ST.get "nonexistent" Map.empty) hdl appInit)
    

------------------------------------------------------------------------------
chooseCompiled :: Assertion
chooseCompiled = do
  let hdl = with heist $ HS.chooseMode
            (liftIO $ return ())
            (liftIO $ assertFailure "Should have chosen compiled mode")
  res <- evalHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) return res


------------------------------------------------------------------------------
chooseInterpreted :: Assertion
chooseInterpreted = do
  let hdl = with heist $ HS.chooseMode
            (liftIO $ assertFailure "Should have chosen intpreted mode")
            (liftIO $ return ())
  res <- evalHandler Nothing (ST.get "" Map.empty) hdl
         (appInit' True False)
  either (assertFailure . show) return res


------------------------------------------------------------------------------
fooRenderWith :: Assertion
fooRenderWith = do
  let mySplices = ("aSplice" ## I.textSplice "Content")
      hdl = with heist $ HS.renderWithSplices "foopage" mySplices
  res  <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  rStr <- either (const $ return "") ST.responseToString res
  assertBool "Splice was not spliced in" (BSC.isInfixOf "Content" (rStr :: BSC.ByteString))


------------------------------------------------------------------------------
seeLocalSplices :: Assertion
seeLocalSplices = do
  let mySplices = do
        "aSplice" ## I.textSplice "Content"
        "bSplice" ## I.textSplice "BContent"
      hdl = with heist $
            HS.withSplices mySplices (HS.withHeistState H.spliceNames)
  res <- evalHandler Nothing (ST.get "" Map.empty) hdl appInit
  either
    (assertFailure . show)
    (\r -> assertBool "Local splices not stored" $
           all (`elem` r) ["aSplice","bSplice"])
    res
  

------------------------------------------------------------------------------
seeLocalState :: Assertion
seeLocalState = do
  let hdl = with heist $ 
            HS.heistLocal
            (I.addTemplate "tinyTemplate" [XML.TextNode "aNode"] Nothing)
            (HS.withHeistState H.templateNames)
  res <- evalHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show)
    (\r -> assertBool "Local state template not found" $
           "tinyTemplate" `elem` (map head r)) res


------------------------------------------------------------------------------
compiledModuleRender :: Assertion
compiledModuleRender = do
  let hdl = with heist $ C.render "foopage"
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res


------------------------------------------------------------------------------
compiledModuleRenderAs :: Assertion
compiledModuleRenderAs = do
  let hdl = with heist $ C.renderAs "audio/ogg" "foopage"
  res <- runHandler Nothing (ST.get "" Map.empty) hdl appInit
  rStr <- either (\_ -> return "") (ST.responseToString) res
  assertBool "Compiled Heist snaplet response should contain \"audoi/ogg\""
    (BSC.isInfixOf "audio/ogg" rStr)


------------------------------------------------------------------------------
compiledModuleServe :: Assertion
compiledModuleServe = do
  let hdl = with heist $ C.heistServe
  res <- runHandler Nothing (ST.get "foopage" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res


------------------------------------------------------------------------------
compiledModuleServeOne :: Assertion
compiledModuleServeOne = do
  let hdl = with heist $ C.heistServeSingle "foopage"
  res <- runHandler Nothing (ST.get "foopage" Map.empty) hdl appInit
  either (assertFailure . show) ST.assertSuccess res
