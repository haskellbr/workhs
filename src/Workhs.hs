{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Workhs
    (
      defaultMain
    , Tutorial
    , def
    )
  where

import           Cheapskate                    (markdown)
import           Cheapskate.Terminal
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8         as ByteString
import           Data.Conduit
import qualified Data.Conduit.Binary           as Conduit.Binary
import qualified Data.Conduit.List             as Conduit.List
import           Data.Conduit.Process
import qualified Data.Conduit.Text             as Conduit.Text
import           Data.Default
import           Data.List
import           Data.Monoid
import           Data.String.Here
import           Data.String.Here.Interpolated
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           System.Console.ANSI
import           System.Console.ListPrompt
import           System.Directory
import           System.Environment            (getArgs, getProgName)
import           System.Exit
import           System.FilePath
import           System.IO.Temp

data Task = Task { taskTitle       :: Text
                 , taskDescription :: Text
                 , taskVerify      :: TaskVerifier
                 }
  deriving(Show)

data TaskVerifier = TaskVerifierIO (FilePath -> IO Bool)

instance Show TaskVerifier where
  show (TaskVerifierIO _) = "TaskVerifierIO"


runVerifier (TaskVerifierIO test) = test

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
                     (TaskVerifierIO (verifyOutput "Hello World"))
               , Task "First functions" "" undefined
               , Task "Introduction to lists" "" undefined
               , Task "List comprehensions" "" undefined
               , Task "Tuples" "" undefined
               ]

verifyOutput :: Text -> FilePath -> IO Bool
verifyOutput out fp = withSystemTempDirectory "workhs" $ \tmp -> do
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

data Tutorial = Tutorial { title     :: Text
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
                    putStrLn "Congratilations! Your program compiled and passed the tests!"
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
