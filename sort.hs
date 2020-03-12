sorte :: (Ord a) => [a] -> [a]
sorte [] = []
sorte (x:xs) =
    let left_sort = sorte [a | a <- xs, a <= x]
        right_sort = sorte [a | a <- xs, a > x]
    in left_sort ++ [x] ++ right_sort