{-  
    Нахождение самых частотных элементов в списке

    Хомич Павел 
    518/2
-}

-- Поиск уникальных элементов в списке
uni :: [Int] -> [Int]
uni [] = []
uni (x:[]) = [x]
uni (x:xs) = x : (uni $ filter (/= x) xs)

-- Нахождение частотности в списке (проход по уникальным, а поиск по исходному!)
freq :: [Int] -> [(Int, Int)]
freq [] = []
freq x = map (\e -> (e, length $ filter (== e) x)) (uni x)

-- Нахождение максимальной частоты в частотном списке
maxFreq :: [(Int, Int)] -> Int
maxFreq [(x,y)] = y
maxFreq (x:xs)
    | snd x > max = snd x
    | otherwise = max
    where max = maxFreq xs

-- Функция нахождения самых частотных чисел в массиве
listFreq :: [Int] -> [Int]
listFreq x =
    let maxF = maxFreq $ freq x  -- максимальная частота в массиве
    in map fst (filter (\(z,zs) -> zs == maxF) (freq x)) -- фильтруем весь список пар по максимальной частоте; берем fst от них
