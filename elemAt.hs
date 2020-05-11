elemAt :: [a] -> Int -> a
elemAt (x:xs) 0 = x
elemAt (x:xs) n = elemAt xs (n - 1)