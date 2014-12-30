module Engager.Interface (exerciseList) where

import Engager.Exercise               
import Graphics.Vty.Input             (Key(..), Modifier(..))
import Graphics.Vty.Widgets.Centering (centered)
import Graphics.Vty.Widgets.Core      (newFocusGroup, addToFocusGroup, onKeyPressed)
import Graphics.Vty.Widgets.EventLoop (Collection, newCollection, addToCollection)
import Graphics.Vty.Widgets.List      (newTextList)
import System.Exit                    (exitSuccess)

exerciseList :: [Exercise] -> IO Collection
exerciseList exercises = do 
                           list <- newTextList (map exerciseTitle exercises) 1
                           ui <- centered list
                           fg <- newFocusGroup
                           fg `onKeyPressed` \_ key modifiers -> if key == KChar 'q' && MCtrl `elem` modifiers
                                                                  then exitSuccess
                                                                  else return False
                           collection <- newCollection
                           _ <- addToCollection collection ui fg 
                           _ <- addToFocusGroup fg list
                           return collection
