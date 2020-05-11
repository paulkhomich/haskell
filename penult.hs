penult :: [a] -> a
penult (x:y:[]) = x
penult (x:xs) = penult xs