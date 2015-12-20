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
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson                   (Value)
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8        as ByteString
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit.Binary
import qualified Data.Conduit.List            as Conduit.List
import           Data.Conduit.Process
import qualified Data.Conduit.Text            as Conduit.Text
import           Data.Default
import           Data.Frontmatter
import           Data.List
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid
import           Data.String.Here
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Encoding           as Text
import           Instances.TH.Lift            ()
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           System.Console.ANSI
import           System.Console.ListPrompt
import           System.Environment           (getArgs, getProgName)
import           System.Exit
import           System.FilePath
import           System.IO.Temp

data Task = Task { taskTitle       :: Text
                 , taskDescription :: Text
                 , taskVerify      :: TaskVerifier
                 }
  deriving(Show)

-- frontmatterMarkdown :: ByteString -> (Maybe Value, Text)
-- frontmatterMarkdown i = case parseYamlFrontmatter i of
--     Done ri ya -> (ya, Text)
--     Fail i' _ _ -> (Nothing, markdown def (Text.decodeUtf8 i'))

readTaskQ :: FilePath -> Q Exp
readTaskQ f = do
    addDependentFile f

    -- Parse the frontmatter out of the markdown file
    taskIn <- runIO (ByteString.readFile f)
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

instance Show TaskVerifier where
  show (TaskVerifierIO _) = "TaskVerifierIO"

runVerifier :: TaskVerifier -> FilePath -> IO Bool
runVerifier (TaskVerifierIO test) = test

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

verifyOutput :: Text -> TaskVerifier
verifyOutput out = TaskVerifierIO $ \fp -> withSystemTempDirectory "workhs" $ \tmp -> do
    (ClosedStream, fromProcess, ClosedStream, cph) <- streamingProcess
        (shell ("stack ghc -- -o " <> (tmp </> "workhs-verify") <> " " <> fp))
    fromProcess
        =$= Conduit.Binary.lines
        $$ Conduit.List.mapM_ $ \l -> do
            setSGR [SetColor Foreground Vivid Blue]
            putStr ("[stack ghc " <> takeFileName fp <> "] ")
            setSGR [Reset]
            ByteString.putStrLn l
    e <- waitForStreamingProcess cph
    case e of
        ExitFailure _ -> error "Failed to compile"
        ExitSuccess -> runResourceT $ do
            (ClosedStream, fromProcess', ClosedStream, cph') <- streamingProcess
                (shell (tmp </> "workhs-verify"))
            out' <- fromProcess'
                =$= Conduit.Binary.lines
                =$= Conduit.List.iterM (\l -> liftIO $ do
                    setSGR [SetColor Foreground Vivid Yellow]
                    putStr ("[" <> takeFileName fp <> "] ")
                    setSGR [Reset]
                    ByteString.putStrLn l)
                =$= Conduit.Text.decodeUtf8
                $$ Conduit.List.fold (<>) mempty
            e' <- waitForStreamingProcess cph'
            return (e' == ExitSuccess && out == out')

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

defaultMain :: Tutorial -> IO ()
defaultMain Tutorial{..} = do
    as <- getArgs
    case as of
        "verify":fp:_ -> do
            -- TODO read state from $HOME/.config
            let currentTask = head tasks
            valid <- runVerifier (taskVerify currentTask) fp
            if valid
                then do
                    setSGR [SetColor Foreground Vivid Green]
                    putStrLn "Congratulations! Your program compiled and passed the tests!"
                    setSGR [Reset]
                    prog <- getProgName
                    putStrLn $ "Type " <> prog <> " to see the next step"
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn "Ops... Something went wrong..."
                    setSGR [Reset]
        [] -> do
            prog <- getProgName
            -- TODO list-prompt should use Text
            taskt <- Text.pack <$> simpleListPrompt def (map (Text.unpack . taskTitle) tasks)
            let Just task = find ((== taskt) . taskTitle) tasks
                md = Text.unlines [ "# " <> title
                                  , "## " <> taskTitle task
                                  , taskDescription task
                                  , footer prog
                                  ]
            prettyPrint (markdown def md)
        _ -> error "Failed to parse arguments"
