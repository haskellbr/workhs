{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Workhs
    (
      defaultMain
    , Options
    , def
    )
  where

import           Cheapskate                    (markdown)
import           Cheapskate.Terminal
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString               as ByteString
import           Data.Conduit
import qualified Data.Conduit.Binary           as Conduit.Binary
import qualified Data.Conduit.List             as Conduit.List
import           Data.Conduit.Process
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
    fromProcess $$ Conduit.List.mapM_ $ \l -> do
        setSGR [SetColor Foreground Vivid Blue]
        putStr "[ghc] "
        setSGR [Reset]
        ByteString.putStr l
    e <- waitForStreamingProcess cph
    case e of
        ExitFailure _ -> error "Failed to compile"
        ExitSuccess -> runResourceT $ do
            (ClosedStream, fromProcess', ClosedStream, cph') <- streamingProcess
                (shell (tmp </> "workhs-verify"))
            fromProcess'
                =$= (Conduit.List.iterM $ \l -> liftIO $ do
                    setSGR [SetColor Foreground Vivid Yellow]
                    putStr ("[" <> dropExtension (takeFileName fp) <> "] ")
                    setSGR [Reset]
                    ByteString.putStr l)
                $$ Conduit.Binary.sinkFile (tmp </> "output")
            e' <- waitForStreamingProcess cph'
            return (e' == ExitSuccess)

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
