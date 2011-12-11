{-# LANGUAGE TemplateHaskell #-}
module Main where

------------------------------------------------------------------------------
import           Data.Char
import           Data.List
import qualified Data.ByteString.Char8 as S
import qualified Data.Text as T
import           Snap.Http.Server (snapServerVersion)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Console.GetOpt
import           System.FilePath
------------------------------------------------------------------------------
import           Snap.StarterTH


------------------------------------------------------------------------------
-- Creates a value tDir :: ([String], [(String, String)])
buildData "tDirBareBones" "barebones"
buildData "tDirDefault" "default"
buildData "tDirTutorial" "tutorial"


------------------------------------------------------------------------------
usage :: String
usage = unlines
    [ "Snap " ++ (S.unpack snapServerVersion) ++ " Project Kickstarter"
    , ""
    , "Usage:"
    , ""
    , "  snap <action>"
    , ""
    , "    <action> can be one of:"
    , "      init - create a new project directory structure in the " ++
        "current directory"
    , ""
    , "  Note: you can use --help after any of the above actions to get help "
    , "  on that action"
    ]


------------------------------------------------------------------------------
initUsage :: String
initUsage = unlines
    [ "Snap " ++ (S.unpack snapServerVersion) ++ " Project Kickstarter"
    , ""
    , "Usage:"
    , ""
    , "  snap init [type]"
    , ""
    , "    [type] can be one of:"
    , "      default   - A default project using snaplets and heist"
    , "      barebones - A barebones project with minimal dependencies"
    , "      tutorial  - The literate Haskell tutorial project"
    , ""
    , "  If [type] is omitted, the default project is generated."
    ]


------------------------------------------------------------------------------
printUsage :: [String] -> IO ()
printUsage ("init":_) = putStrLn initUsage
printUsage _ = putStrLn usage


------------------------------------------------------------------------------
-- Only one option for now
data Option = Help
  deriving (Show, Eq)


------------------------------------------------------------------------------
setup :: String -> ([FilePath], [(String, String)]) -> IO ()
setup projName tDir = do
    mapM createDirectory (fst tDir)
    mapM_ write (snd tDir)
  where
    --------------------------------------------------------------------------
    write (f,c) =
        if isSuffixOf "foo.cabal" f
          then writeFile (projName ++ ".cabal") (insertProjName $ T.pack c)
          else writeFile f c

    --------------------------------------------------------------------------
    isNameChar c = isAlphaNum c || c == '-'

    --------------------------------------------------------------------------
    insertProjName c = T.unpack $ T.replace
                           (T.pack "projname")
                           (T.pack $ filter isNameChar projName) c


------------------------------------------------------------------------------
initProject :: [String] -> IO ()
initProject args = do
    case getOpt Permute options args of
      (flags, other, [])
          | Help `elem` flags -> printUsage other >> exitFailure
          | otherwise         -> go other

      (_, other, errs) -> do putStrLn $ concat errs
                             printUsage other
                             exitFailure

  where
    --------------------------------------------------------------------------
    options =
        [ Option ['h'] ["help"]       (NoArg Help)
                 "Print this message"
        ]

    --------------------------------------------------------------------------
    go ("init":rest) = init' rest
    go _ = do
        putStrLn "Error: Invalid action!"
        putStrLn usage
        exitFailure

    --------------------------------------------------------------------------
    init' args' = do
        cur <- getCurrentDirectory
        let dirs     = splitDirectories cur
            projName = last dirs
            setup'   = setup projName

        case args' of
          []            -> setup' tDirDefault
          ["barebones"] -> setup' tDirBareBones
          ["default"]   -> setup' tDirDefault
          ["tutorial"]  -> setup' tDirTutorial
          _             -> do
            putStrLn initUsage
            exitFailure


------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    initProject args
