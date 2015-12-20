{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Workhs

tutorial :: Tutorial
tutorial = Tutorial { title = "Learn you Haskell!"
                    , description = "Learn the basics of the Haskell programming language"
                    , tutorialId = "learnyouhaskell"
                    , tasks = [ [readTask|./bin/LearnYouHaskell/Hello World.md|]
                                { taskVerify = verifyOutput "Hello World"
                                }
                              ]
                    }

main :: IO ()
main = defaultMain tutorial
