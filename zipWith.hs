zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' op (x:xs) (y:ys) = op x y : zipWith' op xs ys