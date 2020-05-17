collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | odd n     = n : collatz (n*3 + 1)
    | otherwise = n : collatz (n `div` 2)