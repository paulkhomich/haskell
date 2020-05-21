{-  
    Нахождение ранга матрицы произвольного размера

    Хомич Павел 
    518/2
-}

-- Удаление элемента по индексу
dele :: Int -> [a] -> [a]
dele 0 (x:xs) = xs
dele n (x:xs) = x : dele (n - 1) xs

-- Определитель матрицы
det :: [[Int]] -> Int
det [[x]] = x
det [[x, y], [xl, yl]] = x * yl - y * xl
det (y:ys) = 
    let l       = length y - 1    -- Индексы столбцов матрицы
        minors  = (map (\k -> ((-1) ^ k) * det ( map (\x -> dele k x) ys )) [0 .. l]) -- (-1)^k * det minor
    in            sum $ zipWith (*) y minors -- Суммируем произведения 

-- Все комбинации длины n в списке 
comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb n x = 
    let l               = length x - 1 -- Все индексы чисел в списке
        subcomb n id    = comb (n-1) $ drop (id+1) x -- Все комбинации подсписка
        connect n id    = map (\xs -> (x !! id) : xs) $ subcomb n id -- Прибавление элемента ко всем комбинация подсписка
    in                    foldr (++) [] $ map (\id -> connect n id) [0 .. l] -- Склеивание всех подсписков

-- Склеивание матрицы из выбранных n строк
createMatrix :: [[[Int]]] -> [[[Int]]]
createMatrix lines = 
    let l   = (length $ lines !! 0) - 1 -- Длина первого элемента = Количеству итоговых матриц 
    in        map (\i -> map (\el -> el !! i) lines) [0..l] -- Склеиваем в матрицы их кусочки в строках

-- Проверить все детерминанты всех матриц на равенство нулю
allDetsIsZero :: [[Int]] -> Bool 
allDetsIsZero dets = all (== 0) $ foldr (++) [] dets

-- Вычисление ранга матрицы
rank :: [[Int]] ->  Int
rank [] = 0
rank x = 
    let n                   = length x -- кол-во строк
        m                   = length $ x !! 0 -- кол-во столбцов
        maxrank             = min n m -- максимально возможный ранг матрицы (по определению)
        alldets size matrix = map (\lines -> map det $ createMatrix $ map (comb size) lines) $ comb size matrix -- все определители для подматриц размера N
    in                      length $ takeWhile (==False) $ map (\size -> allDetsIsZero $ alldets size x) [1..maxrank] -- Пока не встретим пустой набор определителей
