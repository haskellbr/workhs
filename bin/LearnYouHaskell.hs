{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Workhs

tutorial :: Tutorial
tutorial = Tutorial { title = "Learn you Haskell!"
                    , description = "Learn the basics of the Haskell programming language"
                    , tutorialId = "learnyouhaskell"
                    , tasks = [ [readTask| "hello-world.hs" |]
                              ]
                    }
  where
    ts = [ helloWorldTask
         ]

main :: IO ()
main = defaultMain tutorial
