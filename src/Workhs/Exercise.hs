module Workhs.Exercise (Exercise(..)) where

import           Data.Text (Text)
import           Prelude   (Either, IO)

data Exercise = Exercise { exerciseTitle     :: Text
                         , exerciseStatement :: Text
                         , exerciseVerifier  :: IO (Either [Text] Text) }
