import System.Environment
main = do
    as <- getArgs
    let asNums = map read as :: [Int]
    print (sum asNums)
    print (average asNums)

average :: [Int] -> Double
average l = fromIntegral (sum l) / fromIntegral (length l)
