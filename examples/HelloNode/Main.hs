{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map.Strict (fromList)
import Data.Text       (Text)
import Engager.Core
import Engager.Exercise
import System.Process  (readProcessWithExitCode)

helloWorld :: (Text, Exercise)
helloWorld = ( "hello-world"
             , Exercise "Hello World" 
                        "Create a file called \"hello-world.js\" and Write a program that prints \"hello world\" to the console."
                        (do (_, out, _) <- readProcessWithExitCode "node" ["hello-world.js"] ""
                            return $ if out == "hello world\n"
                                        then Right "Well done!"
                                        else Left ["Oops! Something went wrong."]))

main :: IO ()
main = run $ fromList [ helloWorld ]
