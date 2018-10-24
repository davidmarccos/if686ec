--drop' :: [a] -> [a]
--drop' [] = []
--drop' (x:xs) = if x == 1 then xs else drop' (x-1) xs 

delete                  :: Eq a => a -> [a] -> [a]
delete                  =  deleteBy (==)

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys
