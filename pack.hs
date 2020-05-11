pack :: [Char] -> [String]
pack (x:y:ys)
    | x == y = ([x] ++ y) : pack 