dele :: Int -> [Int] -> [Int]
dele n x = take n x ++ drop (n+1) x

f :: [[Int]] -> Int
f [[x]] = x
f [[x, y], [xl, yl]] = x * yl - y * xl;
f (y:ys) = sum ( zipWith (*) y (map (\z -> ((-1) ^ z) * f ( map (\x -> dele z x) ys )) [0 .. (length y - 1)]))
