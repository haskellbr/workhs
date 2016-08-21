{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Workhs
    (
      -- * Tutorial construction basics
      defaultMain
    , readTask
    , verifyOutput
    , verifyOutputWith
    , verifySpec
      -- * Types
    , Tutorial(..)
    , Task(..)
    , TaskVerifier(..)
      -- * Re-exports
    , def
    , here
    , hereFile
    )
  where

import           Cheapskate                   (markdown)
import           Cheapskate.Terminal
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8        as ByteString.Char8
import qualified Data.ByteString.Lazy         as ByteString.Lazy
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit.Binary
import qualified Data.Conduit.List            as Conduit.List
import           Data.Conduit.Process
import qualified Data.Conduit.Text            as Conduit.Text
import           Data.Default
import           Data.Frontmatter
import qualified Data.HashMap.Strict          as HashMap
import           Data.List
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid
import           Data.String.Here
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Encoding           as Text
import qualified Data.Text.IO                 as Text
import qualified Data.Text.Lazy               as Text.Lazy
import qualified Data.Text.Lazy.IO            as Text.Lazy
import           GHC.Generics
import           Instances.TH.Lift            ()
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           System.Console.ANSI
import           System.Console.ListPrompt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp

data Task = Task { taskTitle       :: Text
                 , taskDescription :: Text
                 , taskVerify      :: TaskVerifier
                 }
  deriving(Show)

readTaskQ :: FilePath -> Q Exp
readTaskQ f = do
    addDependentFile f

    -- Parse the frontmatter out of the markdown file
    taskIn <- runIO (ByteString.Char8.readFile f)
    let (myaml, bmd) = case parseYamlFrontmatter taskIn of
            Done i' ya -> (Just (ya :: Value), i')
            Fail i' _ _ -> (Nothing, i')
            _ -> (Nothing, taskIn)

    -- Try to take the title from the filename by default
    let ftit = Text.pack (dropExtension (takeFileName f))
    -- Use the YAML frontmatter "title" field if it's there
        tit = fromMaybe ftit $ do
            yaml <- myaml
            yaml ^? key "title" . nonNull . _String
    -- Won't do anything with the description now
        des = Text.decodeUtf8 bmd
    [| Task { taskTitle = tit
            , taskDescription = des
            , taskVerify = TaskVerifierIO (const $ return True)
            } |]

readTask :: QuasiQuoter
readTask = QuasiQuoter { quotePat = error "No pattern quoter"
                       , quoteType = error "No type quoter"
                       , quoteDec = error "No declaration quoter"
                       , quoteExp = readTaskQ
                       }

data TaskVerifier = TaskVerifierIO (FilePath -> IO Bool)
                  | TaskVerifierSpec (FilePath -> IO Bool)

instance Show TaskVerifier where
  show (TaskVerifierIO _) = "TaskVerifierIO"
  show (TaskVerifierSpec _) = "TaskVerifierSpec"

runVerifier :: TaskVerifier -> FilePath -> IO Bool
runVerifier (TaskVerifierIO test) = test
runVerifier (TaskVerifierSpec test) = test

taskHelloWorldDescription :: Text
taskHelloWorldDescription = [here|
Write a Haskell program that prints "Hello World" to the console (stdout).

## Tips
To make a Haskell program, create a file with the `.hs` extension. Your file
will have to define a `main` function. You can test your program with
`runhaskell` or `stack runhaskell`. For example:

    $ runghc MyProgram.hs

You can print things to the terminal using `putStrLn`:

    ghci> putStrLn "oh my god!"
|]

defaultTasks :: [Task]
defaultTasks = [ Task "Hello World!" taskHelloWorldDescription
                     (verifyOutput "Hello World")
               , Task "First functions" "" undefined
               , Task "Introduction to lists" "" undefined
               , Task "List comprehensions" "" undefined
               , Task "Tuples" "" undefined
               ]

cabalFileTemplate :: String
cabalFileTemplate = [here|
name:                workhs-example
version:             0.1.0.0
build-type:          Simple
cabal-version:       >=1.10

executable workhs-example
  main-is:             Main.hs
  build-depends:       base >=4.8 && <4.9
  hs-source-dirs:      src
  default-language:    Haskell2010

test-suite hspec
  main-is: Spec.hs
  type: exitcode-stdio-1.0
  build-depends: base
               , hspec
               , QuickCheck
  hs-source-dirs: test
                , src
  default-language: Haskell2010
|]

stackFileTemplate :: String
stackFileTemplate = [here|
resolver: lts-3.19
packages:
- '.'
extra-deps: []
flags: {}
extra-package-dbs: []
|]

hspecSpecTemplate :: String
hspecSpecTemplate = [here|
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
|]

verifySpec :: Either FilePath Text -> TaskVerifier
verifySpec es = case es of
    (Left specFp) -> TaskVerifierSpec $ \fp -> verifySpec' fp =<< Text.readFile specFp
    (Right spec) -> TaskVerifierSpec $ \fp -> verifySpec' fp spec
  where
    verifySpec' :: FilePath -> Text -> IO Bool
    verifySpec' fp spec = withSystemTempDirectory "workhs" $ \tmp -> do
        initSpec tmp spec
        initModule tmp fp
        runSpec tmp
        return False
    runSpec = do
        -- (ClosedStream, fromProcess, ClosedStream, cph) <- streamingProcess
            -- ((shell "stack test") { cwd = Just ""
            --                       })
        return undefined
    initModule tmp fp = do
        createDirectory (tmp </> "src")
        copyFile fp (tmp </> "test" </> "Main.hs")
    initSpec tmp spec = do
        writeFile (tmp </> "workhs-example.cabal") cabalFileTemplate
        writeFile (tmp </> "stack.yaml") stackFileTemplate
        createDirectory (tmp </> "test")
        Text.writeFile (tmp </> "test" </> "MainSpec.hs") spec
        writeFile (tmp </> "test" </> "Spec.hs") hspecSpecTemplate

verifyOutput :: Text -> TaskVerifier
verifyOutput out = verifyOutputWith out []

verifyOutputWith :: Text -> [String] -> TaskVerifier
verifyOutputWith out args = TaskVerifierIO $ \fp -> withSystemTempDirectory "workhs" $ \tmp -> do
    (ClosedStream, fromProcess, stderrFromProcess, cph) <- streamingProcess
        (shell ("stack ghc -- -o " <> (tmp </> "workhs-verify") <> " " <> fp))
    _ <- async $ fromProcess
        =$= Conduit.Binary.lines
        $$ Conduit.List.mapM_ $ \l -> do
            setSGR [SetColor Foreground Vivid Blue]
            putStr ("[stack ghc " <> takeFileName fp <> "] ")
            setSGR [Reset]
            ByteString.Char8.putStrLn l
    _ <- async $ stderrFromProcess
        =$= Conduit.Binary.lines
        $$ Conduit.List.mapM_ $ \l -> do
            setSGR [SetColor Foreground Vivid Red]
            putStr ("[stack ghc " <> takeFileName fp <> "] ")
            setSGR [Reset]
            ByteString.Char8.putStrLn l
    e <- waitForStreamingProcess cph
    case e of
        ExitFailure _ -> error "Failed to compile"
        ExitSuccess -> runResourceT $ do
            (ClosedStream, fromProcess', ClosedStream, cph') <- streamingProcess
                (shell (tmp </> "workhs-verify" <> " " <> unwords args))
            out' <- fromProcess'
                =$= Conduit.Binary.lines
                =$= Conduit.List.iterM (\l -> liftIO $ do
                    setSGR [SetColor Foreground Vivid Yellow]
                    putStr ("[" <> takeFileName fp <> "] ")
                    setSGR [Reset]
                    ByteString.Char8.putStrLn l)
                =$= Conduit.Text.decodeUtf8
                $$ Conduit.List.fold (\t m -> m : t) []
            e' <- waitForStreamingProcess cph'
            let out'' = case out' of [] -> ""; _ -> Text.init (Text.unlines (reverse out')) :: Text
            return (e' == ExitSuccess && out == out'')

data Tutorial = Tutorial { title       :: Text
                           -- ^ The title for your tutorial
                         , description :: Text
                           -- ^ The description for your tutorial in markdown
                         , tasks       :: [Task]
                           -- ^ The task list for you tutorial
                         , tutorialId  :: String
                           -- ^ An ID for your tutorial
                         }
  deriving(Show)

instance Default Tutorial where
    def = Tutorial { title = "My Tutorial"
                   , description = "# You'll be challenged to complete this!"
                   , tasks = defaultTasks
                   , tutorialId = "workhs-default"
                   }

footer :: String -> Text
footer prog = Text.unlines [ "When you're finished with your code, type:"
                           , ""
                           , "    $ " <> Text.pack prog <> " verify MyProgram.hs"
                           , ""
                           , "to continue."
                           ]

data TutorialState = TutorialState { tutorialCompletedTasks :: Value
                                   , tutorialCurrentTask    :: Maybe Text
                                   }
  deriving(Show, Generic)

instance Default TutorialState where
    def = TutorialState (object []) Nothing

instance ToJSON TutorialState
instance FromJSON TutorialState

getStatePath :: IO FilePath
getStatePath = do
    home <- getHomeDirectory
    prog <- getProgName
    createDirectoryIfMissing True (home </> ".config" </> "workhs")
    return $ home </> ".config" </> "workhs" </> (prog <> ".json")

writeState :: TutorialState -> IO ()
writeState st = do
    pth <- getStatePath
    ByteString.Lazy.writeFile pth (encode st)

readState :: IO TutorialState
readState = do
    pth <- getStatePath
    exists <- doesFileExist pth
    if exists
        then do
            contents <- ByteString.Lazy.readFile pth
            case decode contents of
                Just st -> return st
                Nothing -> error "Failed to parse tutorial state"
        else return def

setCurrent :: Task -> TutorialState -> TutorialState
setCurrent Task{..} t = t { tutorialCurrentTask = Just taskTitle
                          }

setCompleted :: Task -> TutorialState -> TutorialState
setCompleted Task{..} t = t { tutorialCompletedTasks =
                                  let Object h = tutorialCompletedTasks t
                                  in Object (HashMap.insert taskTitle (Bool True) h)
                            }

defaultMain :: Tutorial -> IO ()
defaultMain Tutorial{..} = do
    as <- getArgs
    st <- readState
    prog <- getProgName
    case as of
        "verify":fp:_ -> do
            -- TODO read state from $HOME/.config
            let currentTaskTitle = tutorialCurrentTask st
            currentTask <- case currentTaskTitle of
                Nothing -> do
                    hPutStrLn stderr "Not currently working on any task"
                    hPutStrLn stderr $ "Run " <> prog <> " to start a task"
                    exitFailure
                Just taskt -> case find ((== taskt) . taskTitle) tasks of
                    Nothing -> do
                        hPutStrLn stderr "Invalid task in state file:"
                        hPutStrLn stderr $ "  " <> show taskt
                        exitFailure
                    Just t -> return t

            valid <- runVerifier (taskVerify currentTask) fp
            if valid
                then do
                    setSGR [SetColor Foreground Vivid Green]
                    putStrLn "Congratulations! Your program compiled and passed the tests!"
                    setSGR [Reset]
                    let st' = setCompleted currentTask st
                        st'' = case findIndex ((== taskTitle currentTask) . taskTitle) tasks of
                            Just idx -> case tasks ^? element idx of
                                Just t -> setCurrent t st'
                                Nothing -> st'
                            Nothing -> st'
                    writeState st''
                    putStrLn $ "Type " <> prog <> " to see the next step"
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn "Ops... Something went wrong..."
                    setSGR [Reset]
        [] -> do
            -- TODO list-prompt should use Text
            taskt <- Text.pack <$> (simpleListPrompt (listPromptOpts st tasks)
                                    (map (Text.unpack . taskTitle) tasks) >>= \case
                                         Nothing -> error "No selection!"
                                         Just x -> return x)
            let Just task = find ((== taskt) . taskTitle) tasks
                md = Text.unlines [ "# " <> title
                                  , "## " <> taskTitle task
                                  , taskDescription task
                                  , footer prog
                                  ]
            desc <- renderIOWith def { prettyPrintWidth = 78 } (markdown def md)
            forM_ (Text.Lazy.lines desc) $ \l -> do
                putStr "  "
                Text.Lazy.putStrLn l
            let st' = setCurrent task st
            writeState st'
        _ -> error "Failed to parse arguments"

listPromptOpts :: TutorialState -> [Task] -> ListPromptOptions
listPromptOpts st tasks = def { mputChoice = Just put
                           , selectedItemSGR = [ SetColor Foreground Dull Cyan
                                               , SetConsoleIntensity BoldIntensity
                                               ]
                           , normalItemSGR = [ SetColor Foreground Vivid Black
                                             , SetColor Background Vivid White
                                             ]
                           , mlistFooter = Just $
                                 replicate 51 ' ' <>
                                 setSGRCode [ SetColor Foreground Dull White ] <>
                                 "Made with " <>
                                 setSGRCode [ SetColor Foreground Vivid Red ] <>
                                 "❤" <>
                                 setSGRCode [ SetColor Foreground Dull White ] <>
                                 " by " <>
                                 setSGRCode [ SetColor Foreground Vivid Black ] <>
                                 "Haskell" <>
                                 setSGRCode [ SetColor Foreground Vivid Green ] <>
                                 "BR   "
                           }
  where
    put PutChoiceOptions{..} = do
        let task = find ((== putChoiceStr) . Text.unpack . taskTitle) tasks
        case task of
            Nothing -> putStr (putChoiceStr ++ putChoiceSuffix)
            Just t -> case tutorialCompletedTasks st ^? key (taskTitle t) . _Bool of
                Just True -> do
                    setSGR [SetColor Foreground Vivid Green]
                    putStr "◉  "
                    setSGR putChoiceItemSgr
                    putStr putChoiceStr
                    putStr (drop 3 putChoiceSuffix)

                _ -> do
                    setSGR [SetColor Foreground Dull Red ]
                    putStr "◉  "
                    setSGR putChoiceItemSgr
                    putStr putChoiceStr
                    setSGR [SetColor Foreground Dull Yellow ]
                    putStr "  (pending)"
                    putStr (drop (length ("  (pending)" :: String) + 3) putChoiceSuffix)
