module Engager where

data Exercise = Exercise { exerciseTitle     :: String
                         , exerciseStatement :: String
                         , exerciseVerifier  :: String -> Either [String] String }

run :: [Exercise] -> IO ()
run []     = putStr ""
run (x:xs) = do putStrLn (exerciseTitle x)
                run xs
