module Engager.Core where

import Engager.Exercise               (Exercise)
import Engager.Interface              (exerciseList)
import Graphics.Vty.Widgets.Core      (defaultContext)
import Graphics.Vty.Widgets.EventLoop (runUi)

run :: [Exercise] -> IO ()
run exercises = do mainScreen <- exerciseList exercises
                   runUi mainScreen defaultContext
