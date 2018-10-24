{-Exercícios Práticos 2

1) Um método bastante conhecido para encontrar aproximações de raízes
de uma função real é o método de Newton. O método consiste em, dada
uma função f : R → R diferenciável, realizar uma estimativa inicial x​0 para a
raíz e, a partir disso, realizar sucessivas aproximações melhores para a
raíz seguindo a seguinte definição recursiva:
x​n + 1 = x​n- f(x​n)/f’(x​n)

Implemente uma função newton que recebe uma função f :: Double
-> Double​, uma estimativa inicial x0 :: Double e uma precisão eps ::
Double e retorna uma boa aproximação para uma raíz de f. Uma boa
aproximação, neste caso, será o primeiro x​n

tal que |x​n - x​n - 1| < eps.
Considere a seguinte aproximação para a derivada da função f:

f’(x​0) = (f(x​0+ delta) - f(x​0)) / delta, delta = 1e-6

2) Determine o tipo das funções abaixo mostrando os passos até obter o
resultado. Caso não seja possível determinar o tipo, explique por quê.
a) ( . ) thrice map
b) swap map thrice
c) tail . head

Dados:
( . ) :: (b -> c) -> (a -> b) -> a -> c
map :: (a -> b) -> [a] -> [b]
thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))
swap :: a -> (a -> b) -> b
swap f g = g f

3) Em 2018, a escolha do melhor filme do Oscar seguiu um sistema de
votação preferencial, que funciona da seguinte forma: cada votante faz
um ranking dos indicados, de acordo com sua preferência. Se o filme que
aparece mais vezes em primeiro lugar tem mais da metade dos primeiros
lugares, ele é o vencedor. Caso contrário, o filme com menos primeiros
lugares é eliminado (ou seja, retirado dos rankings de cada votante) e o
processo é repetido com os novos rankings até que se obtenha um
vencedor.
Suponha que um ranking de um votante seja representado por uma lista
de filmes, em que o filme na posição 0 é o primeiro lugar. Por exemplo, a
lista [“The Shape of Water”, “Dunkirk”, “Get Out”] significa que o jurado
escolheu The Shape of Water como melhor filme, Dunkirk como segundo
melhor filme e Get Out como terceiro melhor filme.
Escreva uma função winner que, dado uma lista de listas de filmes
representando os rankings de cada jurado, indique quem foi o filme
vencedor. Se houver dois filmes empatados com menos primeiros
lugares, elimine aquele com menos segundos lugares. Se novamente os
dois filmes empataram, elimine aquele com menos terceiros lugares e
assim por diante. Considere que sempre será possível realizar o
desempate.

winner [[“The Shape of Water”, “Dunkirk”, “Get Out”],
[“Get Out”, “Dunkirk”, “The Shape of Water”],
[“Dunkirk”, “The Shape of Water”, “Get Out”]
= “Dunkirk”

-}
-- Q1
derivate :: (Double -> Double) -> (Double -> Double) -- recebe f e retorna a derivada da função 
derivate f = (\x -> ((f (x+delta)) - (f x))/delta)
    where delta = 0.000001

formula :: (Double -> Double) -> Double -> Double -> Double -> Double
formula f x0 eps xPrev | abs(xNow - xPrev) < eps = xNow
                       | otherwise = formula f xNow eps x0
    where   
        xNow = calc f x0
        calc f x = x - ((f x)/(derivate f x)) 

newton :: (Double -> Double) -> Double -> Double -> Double
newton f x0 eps = formula f x0 eps x0

-- Q2
{- A) Como thrice apenas pega uma função, que neste caso é o map, 
		e aplica ela 3 vezes a certo argumento ele apenas vai esperar
		a função de entrada (f) e o valor de "x" que será usado como entrada 
		para função(x). Com isso, sabemeremos que o map f x será executado 
		3 vezes, e a função f será mapeada para cada elemento de x. 
		Assim: (.) thrice map :: (a->a) -> [a] -> [a]
	B) Não é possivel dizer, pois dará um erro na interpretação. Isso pois,
		após o swap trocar a ordem das duas funções, o thrice estará
		esperando uma função que o tipo de entrada seja diferente do 
		tipo de saída (a -> b), enquanto o map é da forma (a->a).
	
	C) Nesse caso, também não é possivel dizer devido à um erro. Isso pois,
		primeiramente, ela receberá uma Lista e pegará apenas o primeiro elemento,
		devido à função head. Após isso, ela tentará usar este elemento como entrada 
		para a função tail, que retornará um erro para o usuário, pois esta espera 
		novamente uma lista.
-}

-- Q3
getStrsByPos :: Int -> [[String]] -> [String]
getStrsByPos n strs = [w | [(z,w)] <- aws]
	where
		-- Pega cada string da lista de listas e numera pela posição, 
		-- depois filtra, em cada lista, apenas a da posição desejada
		aws = map (filter (\(x,y) -> x==n)) (map (zip [1..]) strs)

countReps :: String -> [String] -> Int
countReps str strs = length [x| x<-strs, x == str] 

removeReps :: Eq a => [a] -> [a]
removeReps [] = []
removeReps (x:xs)   | elem x xs   = removeReps xs
						  | otherwise   = x : removeReps xs
							
zipRepsPos :: Int -> [[String]] -> [(String, Int)]
zipRepsPos n strs = removeReps [(x, countReps x inPos)| x <- inPos]
	where
		inPos = getStrsByPos n strs

getWinner :: [(String, Int)] -> Int -> [String]
getWinner strs length= [x| (x,y) <- strs, y > div (length) 2] 


mine:: Int -> Int -> Int
mine a b = if a<b then a else b

tieBraker :: [String] -> [[String]] -> Int -> [String]
tieBraker winners strs n | (length (biggerIndexes)) == 1 = biggerIndexes --vencedor
								 | (length (biggerIndexes)) == 0 = tieBraker winners strs (n+1)
								 | otherwise = tieBraker biggerIndexes strs (n+1)
		where
			atualRanked = zipRepsPos n strs
			minIndex = foldr1 mine [y| (x,y) <- atualRanked, elem x winners] --pegar o menor dos indexes de quem é winner
			biggerIndexes = [x | (x,y) <- atualRanked, elem x winners, y > minIndex] -- todos os winners que nao tem o menor index


winner :: [[String]] -> [String]
winner strs | (length winners) == 1 = winners
				| (length winners) == 0 = tieBraker (head strs) strs 2
				| otherwise = tieBraker winners strs 2
				
		where
			winners = getWinner (zipRepsPos 1 strs) (length (head strs))