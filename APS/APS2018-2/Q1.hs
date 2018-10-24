quantidade :: Int -> Int -> Int
quantidade num dig = range dig [0..num]
  where
      range _ [] = 0
      range y (x:xs) | x == y = 1 + range y xs
        | x >= y*10 = 1
        | otherwise = range y xs 