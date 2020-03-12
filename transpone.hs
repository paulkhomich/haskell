trnps :: [[Int]] -> [[Int]]
trnps ([]:_) = [[]]
trnps m = map (\(x:xs) -> x) m : trnps (map (\(x:xs) -> xs) m)