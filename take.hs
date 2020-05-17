take' :: Int -> [a] -> [a]
take' n (x:xs) 
    | n <= 0    = [] 
    | otherwise = x : take' (n-1) xs