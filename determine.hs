dele :: Int -> [a] -> [a]
dele 0 (x:xs) = xs
dele n (x:xs) = x : delel (n - 1) xs

det :: [[Int]] -> Int
det [[x]] = x
det [[x, y], [xl, yl]] = x * yl - y * xl
det (y:ys) = sum $ zipWith (*) y (map (\z -> ((-1) ^ z) * det ( map (\x -> dele z x) ys )) [0 .. (length y - 1)])
