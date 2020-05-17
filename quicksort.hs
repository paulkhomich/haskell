quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let big     = filter (>x) xs
        small   = filter (<=x)  xs
    in quicksort big ++ [x] ++ quicksort small