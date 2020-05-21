f :: [[Int]] -> [[Int]] -> [[Int]]
f _ ([]:_) = [[]]
f ([]:_) _ = [[]] 
f (x:[]) y = zipWith (++) [[ (sum (zipWith (*) x (map (\(z:zs) -> z) y))) ]] (f [x] (map (\(z:zs) -> zs) y))
f (x:xs) y = f [x] y ++ f xs y

