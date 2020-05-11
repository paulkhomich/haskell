length' :: [a] -> Int
length' [x] = 1
length' (x:xs) = length' xs + 1