{-  
    Нахождение собственных значений квадратной матрицы (QR-алгоритм)

    Хомич Павел 
    518/2
-}

{-

    ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ

-}
-- Транспонирование матрицы
trnps :: [[Float]] -> [[Float]]
trnps ([]:_) = []
trnps m = map (\(x:xs) -> x) m : trnps (map (\(x:xs) -> xs) m)

-- Умножение матрицы на матрицу
mult :: [[Float]] -> [[Float]] -> [[Float]]
mult _ ([]:_) = [[]]
mult ([]:_) _ = [[]] 
mult (x:[]) y = 
    let fsts y = map (\(z:zs) -> z) y
        snds y = map (\(z:zs) -> zs) y
    in  zipWith (++) [[scalar x (fsts y)]] (mult [x] (snds y))
mult (x:xs) y = mult [x] y ++ mult xs y

-- Скалярное произведение векторов
scalar :: [Float] -> [Float] -> Float
scalar x y = sum $ zipWith (*) x y

-- Умножение вектора на константу
mconst :: Float -> [Float] -> [Float]
mconst n x = map (*n) x

-- Вычитание векторов
sub :: [Float] -> [Float] -> [Float]
sub x y = zipWith (-) x y

-- Суммирование двух векторов
add :: [Float] -> [Float] -> [Float]
add x y = zipWith (+) x y

-- Суммирование n векторов
addN :: [[Float]] -> [Float]
addN xs =
    let l = length (xs !! 0)
    in foldr (add) (replicate l 0.0) xs

-- Промежуточное вычисление при ортогонализации
-- (<a,b>/<b,b>) * b
c :: [Float] -> [Float] -> [Float]
c a b = (scalar a b / scalar b b) `mconst` b

-- Нормирование матрицы (по строкам)
norm :: [[Float]] -> [[Float]]
norm a = 
    let sq v = sqrt $ sum $ map (^2) v 
    in  map (\v -> (if (sq v /= 0.0) then (1 / (sq v)) else 0.0) `mconst` v) a 

{-

    QR-РАЗЛОЖЕНИЕ 

-}
-- Получение вектора в Q разложении по индексу
getQv :: Int -> [[Float]] -> [Float]
getQv 0 a = a !! 0
getQv n a = 
    let an  = a !! n    -- Текущий опорный элемент
        l   = n - 1     -- Размер предыдущих векторов Q в вычислении
    in  an `sub` (addN $ map (\id -> c an (getQv id a)) [0..l])

-- Алгоритм получения Q матрицы из матрицы A
-- В начала и конце транспонируем матрицы, чтобы работать со строками (с чистыми списками удобнее)
getQ :: [[Float]] -> [[Float]]
getQ a = 
    let n   = length a - 1  -- размер матрицы
        m   = trnps a -- транспонируем матрицу для вычисления
    in  trnps $ norm $ foldr (:) [] $ map (\id -> getQv id m) [0..n] -- транспонируем обратно

-- Алгоритм получения R матрицы из матриц Q и А
getR :: [[Float]] -> [[Float]] -> [[Float]]
getR q a = mult (trnps q) a

{-

    QR-АЛГОРИТМ

-}
-- Получение собственных значений матрицы (+ по заданному кол-ву итераций точности)
eigen :: Int -> [[Float]] -> [Float]
eigen 0 a = map (\id -> (a !! id) !! id) [0..length a - 1] -- Берем значения на диагонали
eigen n a = 
    let q  = getQ a     -- Получаем ортогональную матрицу
        r  = getR q a   -- Получаем верхнетрегуольную матрицу
        mm = mult r q   -- Получаем подобную матрицу
    in  eigen (n-1) mm  -- Повтор