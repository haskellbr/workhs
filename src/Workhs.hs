{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Workhs
    (
      defaultMain
    , Options
    , def
    )
  where

import           Cheapskate                (markdown)
import           Cheapskate.Terminal
import           Data.Default
import           Data.List
import           Data.Monoid
import           Data.String.Here
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import           System.Console.ListPrompt
import           System.Environment        (getProgName)

data Task = Task { taskTitle       :: Text
                 , taskDescription :: Text
                 }
  deriving(Show)

taskHelloWorldDescription = [here|
Write a Haskell program that prints "Hello World" to the console (stdout).

## Tips
To make a Haskell program, create a file with the `.hs` extension. Your file
will have to define a `main` function. You can test your program with `runghc`
or `stack runghc`. For example:

    $ runghc MyProgram.hs

You can print things to the terminal using `putStrLn`:

    ghci> putStrLn "oh my god!"
|]

defaultTasks :: [Task]
defaultTasks = [ Task "Hello World!" taskHelloWorldDescription
               , Task "First functions" ""
               , Task "Introduction to lists" ""
               , Task "List comprehensions" ""
               , Task "Tuples" ""
               ]

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

footer = [here|
When you're finished with your code, type:

    $ learnyouhaskell verify MyProgram.hs

to continue.
|]

defaultMain :: Options -> IO ()
defaultMain Options{..} = do
    prog <- getProgName
    -- TODO list-prompt should use Text
    taskt <- Text.pack <$> simpleListPrompt def (map (Text.unpack . taskTitle) tasks)
    let Just task = find ((== taskt) . taskTitle) tasks
        md = Text.unlines [ "# " <> title
                          , "## " <> taskTitle task
                          , taskDescription task
                          , footer
                          ]
    prettyPrint (markdown def md)
