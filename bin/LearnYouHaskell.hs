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
                              , [readTask|./bin/LearnYouHaskell/Baby Steps.md|]
                                { taskVerify =
                                      verifyOutputWith "10\n2.5" (map show [1, 2, 3, 4])
                                }
                              ]
                    }

main :: IO ()
main = defaultMain tutorial
