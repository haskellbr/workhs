{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Workhs
    (
      defaultMain
    , Options
    , def
    )
  where

import           Data.Default
import           Data.Text                 (Text)
import qualified Data.Text.IO              as Text
import           System.Console.ListPrompt

data Task = Task String

data Options = Options { title       :: Text
                       , description :: Text
                       , tasks       :: [Task]
                       }

instance Default Options where
    def = Options { title = "My Tutorial"
                  , description = "# You'll be challenged to complete this!"
                  , tasks = []
                  }

defaultMain :: Options -> IO ()
defaultMain Options{..} = do
    Text.putStrLn title
    Text.putStrLn description
