-- Remover n's el from List
dele :: Int -> [a] -> [a]
dele 0 (x:xs) = xs
dele n (x:xs) = x : dele (n - 1) xs
-- Det
det :: [[Float]] -> Float
det [[x]] = x
det [[x, y], [xl, yl]] = x * yl - y * xl;
det (y:ys) = sum ( zipWith (*) y (map (\z -> ((-1) ^ z) * det ( map (\x -> dele z x) ys )) [0 .. (length y - 1)]))

-- Transposition
trnps :: [[Float]] -> [[Float]]
trnps ([]:_) = []
trnps m = map (\(x:xs) -> x) m : trnps (map (\(x:xs) -> xs) m)

-- Add matrix
addm :: [[Float]] -> [[Float]]
addm m = map (\indU -> map (\ind -> ((-1) ^ (indU + ind)) * det ( dele indU (map (\x -> dele ind x) m) ) ) [0 .. length m - 1]) [0 .. length m - 1]

-- Const mult
multc :: Float -> [[Float]] -> [[Float]]
multc c m = map (map (*c)) m



--Inverse matrix
invm :: [[Float]] -> [[Float]]
invm m = multc (1 / det m) (trnps $ addm $ m)