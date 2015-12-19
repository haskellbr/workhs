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

data Options = Options { title       :: Text
                       , description :: Text
                       }

instance Default Options where
    def = Options { title = "My Tutorial"
                  , description = "# You'll be challenged to complete this!\n"
                  }

defaultMain :: Options -> IO ()
defaultMain Options{..} = do
    Text.putStrLn title
    Text.putStrLn description
