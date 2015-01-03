module Engager.Core where

import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as M
import           Data.Text                      (Text)
import           Engager.Exercise               (Exercise)
import           Engager.Interface              (mainScreen)
import           Graphics.Vty.Widgets.Core      (defaultContext)
import           Graphics.Vty.Widgets.EventLoop (runUi)

run :: Map Text Exercise -> IO ()
run exercises = do screen <- mainScreen $ map snd $ M.toList exercises
                   runUi screen defaultContext
