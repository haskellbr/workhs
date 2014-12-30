module Engager.Exercise (Exercise(..)) where

import Data.Text (Text)
import Prelude   (Either)

data Exercise = Exercise { exerciseTitle     :: Text
                         , exerciseStatement :: Text
                         , exerciseVerifier  :: Text -> Either [Text] Text }
