palindrome :: Eq a => [a] -> Bool
palindrome []       = True
palindrome [_]      = True
palindrome (x:xs)   = (x == last xs) && palindrome (init xs)