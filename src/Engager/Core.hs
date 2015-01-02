module Engager.Core where

import Engager.Exercise               (Exercise)
import Engager.Interface              (mainScreen)
import Graphics.Vty.Widgets.Core      (defaultContext)
import Graphics.Vty.Widgets.EventLoop (runUi)

run :: [Exercise] -> IO ()
run exercises = do screen <- mainScreen exercises
                   runUi screen defaultContext
