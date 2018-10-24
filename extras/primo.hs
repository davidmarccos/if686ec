fatores n = [i | i<-[1..n], n `mod` i == 0]

primo :: Int -> Bool
primo n = if (fatores n) == [1,n] then True else False