quantidade :: Int -> Int -> Int
quantidade x 0 = 0
quantidade x y = contaOc y [0..x]
    where
        contaOc a [] = 0
        contaOc a (x:xs) | x == a = 1 + contaOc a xs
                 | x >= a*10 = 1 + contaOc a xs
                 | otherwise = contaOc a xs