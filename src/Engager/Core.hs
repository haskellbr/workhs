module Engager.Core (run) where

import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as M
import           Data.Text                      (Text)
import           Engager.Exercise               (Exercise)
import           Engager.Interface              (mainScreen)
import           Graphics.Vty.Widgets.Core      (defaultContext)
import           Graphics.Vty.Widgets.EventLoop (runUi)
import           Options.Applicative

data Options = Options { verify :: Maybe String }

options :: Parser Options
options = Options <$> (optional $ strOption (long "verify" <> short   'v'
                                                           <> metavar "Exercise"
                                                           <> help    "Which exercise to verify"))

start :: Map Text Exercise -> Options -> IO ()
start exercises (Options Nothing)         = do screen <- mainScreen $ map snd $ M.toList exercises
                                               runUi screen defaultContext
start exercises (Options (Just exercise)) = putStrLn $ "Verify " ++ exercise

run :: Map Text Exercise -> IO ()
run exercises = do args <- execParser opts
                   start exercises args
  where
    opts = info (helper <*> options)
                (fullDesc <> progDesc ""
                          <> header   "")
