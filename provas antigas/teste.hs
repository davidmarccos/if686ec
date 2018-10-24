{- 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
   do menor para o maior elemento..
   exemplo: isSorted [1,6,8,9,9] ------> True
            isSorted [1,6,8,7,9] ------> False
   Dica: verifique se sua resposta funciona para listas de tamanho ímpar.
-}

isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (a:(b:bs)) | (a <= b)     = (True && (isSorted (b:bs)))
                    | otherwise    = False


substr :: String -> String -> Bool
substr at@(a:as) bt@(b:bs) | (locate a bt) /= -1 = find as bs || substr at bs
                           | otherwise = False
                        where
                          find _ [] = False
                          find [] _ = True
                          find (x:xs) (y:ys) | x==y = find xs ys
                                             | otherwise = False