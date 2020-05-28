-- drop и dropWhile с помощью свертки

drop' :: Int -> [a] -> [a]
drop' i list = foldr (\x a -> if length a < l then x : a else a) [] list
    where l = (length list - i)

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f list = foldl (\a x -> if f x && empty a then a else a ++ [x]) [] list
    where empty a = length a == 0