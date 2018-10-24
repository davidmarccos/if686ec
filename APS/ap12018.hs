{-Exercícios Práticos

1) Escreva uma função ​kSmallest​ que recebe uma lista de inteiros e um
inteiro k e retorna uma lista com os k menores inteiros, preservando a
ordem original em que aparecem.

kSmallest :: [Int] -> Int -> [Int]
kSmallest [1, 5, 3, 2, 0] 3 = [1, 2, 0]

2) Implemente a função ​composites​ que, dado uma lista ​ps ​de números
primos e uma lista ​ns​ de números inteiros, retorna uma lista com os
inteiros de ​ns​ que são formados apenas pelo produto de primos de ​ps​.

composites :: [Int] -> [Int] -> [Int]
composites [2, 3, 7] [4, 5, 6, 7, 10, 14, 15] = [4, 6, 7, 14]

P.S.: O número 1 pode ser considerado o produto de 0 primos.

3) Combinação é um subconjunto com p elementos de um conjunto maior
com n elementos. Crie uma função combinations :: [Int] -> [[Int]] para
gerar todas as possíveis combinações sem repetição dos inteiros de uma
lista de entrada.

combinations [1, 2, 3] = [[], [3], [2], [1], [2, 1], [1, 3], [2, 3], [2, 1, 3]]
combinations [10, -80, 14, 16] =
[[], [10], [-80], [10, -80], [14], [10, 14], [-80, 14], [10, -80, 14], [16], [10,
16], [-80, 16], [10, -80, 16], [14, 16], [10, 14, 16], [-80, 14, 16], [10, -80,
14, 16]]-}

-- Q1:
kSmallest :: [Int] -> Int -> [Int]
kSmallest as z =  onlyFirst (qsortscnd (take z organizedAs))
        where
            qsortfrst [] = []
            qsortfrst ((c,d):ss) = qsortfrst [(x,y) | (x,y)<-ss, x<c] ++ [(c,d)] ++ qsortfrst [(x,y) | (x,y)<-ss, x>=c] 

            qsortscnd [] = []
            qsortscnd ((c,d):ss) = qsortscnd [(x,y) | (x,y)<-ss, y<d] ++ [(c,d)] ++ qsortscnd [(x,y) | (x,y)<-ss, y>=d] 

            organizedAs = qsortfrst (zip as [0..]) -- ordena pelo tamanho

            onlyFirst [] = []
            onlyFirst ((c,d):ss) = [c] ++ onlyFirst ss

-- Q2:
isDivisible :: [Int] -> Int -> Bool
isDivisible [] _ = False
isDivisible pt@(p:ps) a | a == 1 = True
                     | mod a p == 0 = isDivisible pt (div a p)
                     | otherwise = isDivisible ps a


composites :: [Int] -> [Int] -> [Int]
composites ps ns = [x| x <- ns, isDivisible ps x]


-- Q3:
isEqual :: [Int] -> [Int] -> Bool  -- ve se duas listas têm os mesmos algarismos
isEqual [] [] = True
isEqual [] xs = True
isEqual xs [] = False
isEqual (x:xs) ys = length [z| z<-ys, z == x] /= 0 && isEqual xs ys

alreadyCounted :: [Int] -> [[Int]] -> Bool -- checa se alguma lista já existe numa lista de lista baseado em IsEqual
alreadyCounted a bs = length [z| z<-bs, length a == length z && isEqual z a] /= 0

nonRepeated :: [[Int]] -> [[Int]] -- tira as combinações repetidas se baseando nas duas últimas funções
nonRepeated [] = []
nonRepeated [x] = [x]
nonRepeated (x:xs) | alreadyCounted x counted = counted
                   | otherwise = [x] ++ counted
                   where 
                        counted = nonRepeated xs

combinations :: [Int] -> [[Int]]
combinations [] = [[]]
combinations [a] = [[a]] ++ combinations []
combinations (x:xs) = nonRepeated allCombinations --Retira todas as combinações com repetições
    where
        allAnagrams = [take i p ++ [x] ++ drop i p |  p <- combinations xs, i <- [0..length p]] --todos os anagramas
        allCombinations = combinations [x] ++ combinations xs ++ allAnagrams --pega todas as combinações do termo que eu estou,
                                                                            -- do resto da cadeia, e os anagramas envolvendo os dois
        