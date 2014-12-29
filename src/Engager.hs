{-# LANGUAGE OverloadedStrings #-}

module Engager where

import Graphics.Vty.Widgets.All
import Control.Monad (forM_)
import Data.Text

data Exercise = Exercise { exerciseTitle     :: Text
                         , exerciseStatement :: Text
                         , exerciseVerifier  :: Text -> Either [Text] Text }

run :: [Exercise] -> IO ()
run exercises = do exerciseList <- newList 1
                   forM_ exercises (\e -> addToList exerciseList (exerciseTitle e) =<< plainText (exerciseTitle e))
                   ui <- centered exerciseList
                   fg <- newFocusGroup
                   collection <- newCollection
                   _ <- addToCollection collection ui fg 
                   _ <- addToFocusGroup fg exerciseList
                   runUi collection defaultContext
