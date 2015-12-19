{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Workhs
    (
      defaultMain
    , Options
    , def
    )
  where

import           Cheapskate                    (markdown)
import           Cheapskate.Terminal
import           Data.Default
import           Data.List
import           Data.Monoid
import           Data.String.Here
import           Data.String.Here.Interpolated
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           System.Console.ListPrompt
import           System.Directory
import           System.Environment            (getArgs, getProgName)
import           System.Exit
import           System.FilePath
import           System.Process

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
verifyOutput out fp = do
    tmp <- getTemporaryDirectory
    print tmp
    e <- spawnCommand
             ("stack ghc -- -o " <> (tmp </> "workhs-verify") <> " " <> fp) >>=
         waitForProcess
    case e of
        ExitFailure _ -> error "Failed to compile"
        ExitSuccess -> do
            putStrLn (tmp </> "workhs-verify")
            (_, o, _) <- readProcessWithExitCode (tmp </> "workhs-verify") [] ""
            print o
            return True

data Options = Options { title       :: Text
                       , description :: Text
                       , tasks       :: [Task]
                       }
  deriving(Show)

instance Default Options where
    def = Options { title = "My Tutorial"
                  , description = "# You'll be challenged to complete this!"
                  , tasks = defaultTasks
                  }

footer :: String -> Text
footer prog = Text.unlines [ "When you're finished with your code, type:"
                           , ""
                           , "    $ " <> Text.pack prog <> " verify MyProgram.hs"
                           , ""
                           , "to continue."
                           ]

defaultMain :: Options -> IO ()
defaultMain Options{..} = do
    as <- getArgs
    case as of
        "verify":fp:_ -> do
            -- TODO read state from $HOME/.config
            let currentTask = head tasks
            valid <- runVerifier (taskVerify currentTask) fp
            if valid
                then putStrLn "Bazinga!"
                else putStrLn "Ops..."
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
