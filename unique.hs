uni :: [Int] -> [Int]
uni [] = []
uni (x:[]) = [x]
uni (x:xs) = x : uni (filter (\z -> z /= x) xs)
