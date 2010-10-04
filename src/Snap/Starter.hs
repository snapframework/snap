{-# LANGUAGE TemplateHaskell #-}
module Main where

------------------------------------------------------------------------------
import           Data.List
import qualified Data.Text as T
import           System
import           System.Directory
import           System.Console.GetOpt
import           System.FilePath
------------------------------------------------------------------------------

import Snap.StarterTH


------------------------------------------------------------------------------
-- Creates a value tDir :: ([String], [(String, String)])
$(buildData "tDirDefault"   "default")
$(buildData "tDirBareBones" "barebones")
$(buildData "tDirHint"      "hint")

------------------------------------------------------------------------------
usage :: String
usage = unlines
    ["Usage:"
    ,""
    ,"  snap <action>"
    ,""
    ,"    <action> can be one of:"
    ,"      init - create a new project directory structure in the current directory"
    ,""
    ,"  Note: you can use --help after any of the above actions to get help on that action"
    ]


------------------------------------------------------------------------------
data InitFlag = InitBareBones
              | InitHelp
              | InitHint
  deriving (Show, Eq)


setup :: String -> ([FilePath], [(String, String)]) -> IO ()
setup projName tDir = do
    mapM createDirectory (fst tDir)
    mapM_ write (snd tDir)
  where
    write (f,c) =
        if isSuffixOf "foo.cabal" f
          then writeFile (projName ++ ".cabal") (insertProjName $ T.pack c)
          else writeFile f c
    insertProjName c = T.unpack $ T.replace
                           (T.pack "projname")
                           (T.pack projName) c

------------------------------------------------------------------------------
initProject :: [String] -> IO ()
initProject args = do
    case getOpt Permute options args of
      (flags, _, [])
        | InitHelp `elem` flags -> do putStrLn initUsage
                                      exitFailure
        | otherwise             -> init' flags

      (_, _, errs) -> do putStrLn $ concat errs
                         putStrLn initUsage
                         exitFailure
  where
    initUsage = unlines
        ["Usage:"
        ,""
        ,"  snap init"
        ,""
        ,"    -b  --barebones   Depend only on -core and -server"
        ,"    -i  --hint        Depend only on hint for dynamic reloading"
        ,"    -h  --help        Print this message"
        ]

    options =
        [ Option ['b'] ["barebones"] (NoArg InitBareBones)
                 "Depend only on -core and -server"
        , Option ['i'] ["hint"]      (NoArg InitHint)
                 "Depend on hint"
        , Option ['h'] ["help"]      (NoArg InitHelp)
                 "Print this message"
        ]

    init' flags = do
        cur <- getCurrentDirectory
        let dirs = splitDirectories cur
            projName = last dirs
            setup' = setup projName
        case flags of
          (_:_) | InitHint      `elem` flags -> setup' tDirHint
                | InitBareBones `elem` flags -> setup' tDirBareBones
          _                                  -> setup' tDirDefault


------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("init":args') -> initProject args'
        _              -> do putStrLn usage
                             exitFailure
