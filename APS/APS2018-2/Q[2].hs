limpaUm :: [Int] -> [Int]
limpaUm l = [ aux (show x) | x <- l, aux (show x) /= 1]
    where
        aux s | remove s /= "" = read (remove s) :: Int
              | otherwise = 1 
        remove s = [a | a <- s, a /= '1']