-- eduardo
primo :: Int -> Bool
primo x = isPrime 2 x


isPrime :: Int -> Int -> Bool
isPrime x y | x == y = True
            | mod y x == 0 = False
            | otherwise = isPrime (x+1) y
			
-- lucas			
primo :: Integer -> Bool 
primo 1 = False 
primo x | (length [ y | y <- [1..x], x`mod`y == 0, y /=x ]) > 0 = False 
	     | otherwise = True 
		 
-- fernando
somaUm :: (Int, Int) -> (Int, Int)
somaUm (a, b) = (a+1, b)

divisaoEuc :: Int -> Int -> (Int, Int)
divisaoEuc a b
 | a < b = (0, a)
 | otherwise = somaUm(divisaoEuc (a-b) b)
 
 -- eduardo
 divisaoEuclidiana :: Int -> Int -> (Int,Int)
divisaoEuclidiana x y = divisaoCerta y 0 x 

divisaoCerta :: Int -> Int -> Int -> (Int,Int)
divisaoCerta y a b | b < y = (a, b) 
                     | otherwise = divisaoCerta y (a+1) (b-y)
					 
--fernando
inverte :: [a] -> [a]
inverte [] = []
inverte (b:as) = (inverte as)++[b]

pali :: String -> Bool
pali s = s == inverte s

-- victor
palindromo :: String -> Bool
palindromo str | reverse str == str = True
               | otherwise = False
			   
-- juliana
doisadois :: Int -> [(Int,Int)]
doisadois a = dad a 0 0

dad :: Int -> Int -> Int -> [(Int,Int)]
dad a b c | a == b = [(a,b)]
          | a == c = (b,c) : dad a (b+1) (b+1)
          | otherwise = (b,c) : dad a b (c+1)
		  
-- fernando
combina n = [(x,y) | x <- [0..n], y <- [0..n], x <= y]
