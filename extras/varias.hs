--O fatorial de um número inteiro positivo

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

{-
criar uma função que gere uma tabela mostrando o número de todos os alunos e suas respectivas notas. No final da tabela deve
aparecer a média das notas. Exemplo:
Haskell > tabela 4
Aluno Nota
1     7.5
2     10
3     9
4     6.3
-}

tabela :: Int -> String
tabela n = cabecalho ++ imprimeAlunos n ++ imprimeMedia n

cabecalho :: String
cabecalho = "Aluno    Nota\n"

imprimeAlunos :: Int -> String
imprimeAlunos 1 = imprimeAluno 1
imprimeAlunos n = imprimeAlunos (n-1) ++ imprimeAluno n

imprimeAluno :: Int -> String
imprimeAluno n = show n ++ "        " ++ show (aluno n) ++ "\n" 
--show transforma um número de qualquer tipo em string

imprimeMedia :: Int -> String
imprimeMedia n = "\n" ++ "Media da turma: " ++ show (media n)

soma :: Int -> Float
soma 1 = aluno 1
soma n = aluno n + soma (n-1)

media :: Int -> Float
media n = (soma n) / (fromIntegral n)

--banco de alunos
aluno :: Int -> Float
aluno 1 = 7.5
aluno 2 = 10
aluno 3 = 9
aluno 4 = 6.3

---------------------------------

--Somar os elementos de uma lista não vazia

somalista :: [Int] -> Int
somalista [] = 0
somalista (x:xs) = x + somalista xs

---------------------------------

--dobrar os elementos de uma lista não vazia

dobralista :: [Int] -> [Int]
dobralista [] = []
dobralista (x:xs) = x*2 : dobralista xs 

---------------------------------

--dá o número de elementos de uma lista

tamlista :: [t] -> Int
tamlista [] = 0
tamlista (x:xs) = 1 + length xs

---------------------------------

--função de ordenação de uma lista.

ordenalista :: [Int] -> [Int]
ordenalista [] = []
ordenalista (x:xs) = insere x (ordenalista xs)

insere :: Int -> [Int] -> [Int]
insere e [] = [e]
insere e (x:xs)
  | e <= x = e:(x:xs)
  | otherwise = x : insere e xs

---------------------------------

--soma pares de uma lista

somapares :: [(Int, Int)] -> [Int]
somapares lista = [a+b | (a,b) <- lista] --por compreenssao

---------------------------------

--Quando se trabalha com mais de um gerador, o primeiro valor da primeira lista é
--gerado e mantido enquanto se avalia os valores da lista seguinte

pares :: [t] -> [u] -> [(t,u)]
pares n m = [(a,b) | a <- n , b <-m]

{-Haskell > pares [1,2,3] [4,5]
[(1,4), (1,5), (2,4), (2,5), (3,4), (3,5)]-}

---------------------------------

--remove um caracter de uma string

remove :: Char -> [Char] -> [Char]
remove carac str = [c | c <-str , c/= carac]

{-Haskell > remove ‘ ‘ “Este exemplo remove os espaços em branco!”
“Esteexemploremoveosespaçosembranco!”-}

---------------------------------

--Tendo uma lista de tuplas, em que cada tupla tem-se o número do aluno, o nome do
--aluno e sua nota. Pode-se transformar esta lista em uma lista de nomes de alunos:

baseDeDados :: [(Int, String, Float)]
baseDeDados = [ (1, "André", 10.0), (2, "Carlos", 6.8), (3,"Maurício", 7.0)]

nomes :: [(Int, String, Float)] -> [String]
nomes list = [pegaNome a | a <-list]
   where --pegaNome foi definido localmente atraves da palavra reservada where e soh funcionara pra funcao nomes
   pegaNome (a,b,c) = b
   
--ou de uma forma mais simples
--nomes list = [b | (a,b,c) <-list]

---------------------------------

--FOLDR1
{-Haskell > foldr1 (&&) [True, False, True]
False
Haskell > foldr1 (++) [“Concatenar “,“uma “,“lista “,“de ”,“strings “,“em “,“uma “,“só.”]
“Concatenar uma lista de strings em uma só.”
Haskell > foldr1 (+) [1,2,3]
6-}

 
---------------------------------

--função que de os nomes dos alunos com nota maior que 7.

baseDeDados :: [(Int, String, Float)]
baseDeDados = [ (1, "André", 10.0), (2, "Carlos", 6.8), (3,"Maurício", 7.0)]

alunos :: [(Int, String, Float)] -> [String]
alunos base = map pegaNome (filter nota base)
  where
  nota (a,b,c) = c>7
  pegaNome (a,b,c) = b

{-Haskell > alunos baseDeDados
[“André”]-}

---------------------------------

--TAKE DROP
{-
Haskell > take 3 [1, 2, 3, 4, 5, 6]
[1, 2, 3]
Haskell > take 0 [2, 4, 6, 8, 10]
[]
Haskell > drop 3 [2, 4, 6, 8, 10]
[8, 10]
Haskell > drop 10 [2, 4, 6, 8, 10]
[]
-}

---------------------------------

--FILTER MAP FOLDR1

Haskell > filter par [2, 4, 5, 6, 10, 11]
[2, 4, 6, 10]
Haskell > map (2*) [1, 2, 3]
[2, 4, 6]
Haskell > map length [“Haskell”, “Hugs”, ”GHC”]
[7, 4, 3]
Haskell > foldr1 (&&) [True, False, True]
False
Haskell > foldr1 (++) [“Concatenar “,“uma “,“lista “,“de ”,“strings “,“em “,“uma “,“só.”]
“Concatenar uma lista de strings em uma só.”
Haskell > foldr1 (+) [1,2,3]
6

---------------------------------

--ZIP transforma duas listas em uma lista de tuplas.

Haskell > zip [1, 3, 5] [2, 4, 6]
[(1,2), (3, 4), (5, 6)]
Haskell > zip [1, 3, 5, 7, 9, 11] [2, 4, 6]
[(1,2), (3, 4), (5, 6)]

---------------------------------

--

somaOsDoisPrimeiros :: [Int] -> Int
somaOsDoisPrimeiros (a:b:x) = a+b

---------------------------------

--remove pontuacao de um texto

removePontuacao :: String -> String
removePontuacao str = remove ‘!’ (remove ‘.’ ( remove ‘,’ str) ) )

---------------------------------

--se um elemento pertence a lista

elem :: (Eq a) => a -> [a] -> Bool
elem x [] = False
elem x (y:ys) = x == y || (elem x ys)

---------------------------------

--TIPOS ALGEBRICOS

type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

mostraPessoa :: Pessoas -> String
mostraPessoa (Pessoa nom idade) = “Nome: “ ++ nom ++ “ Idade: “ ++ show idade

{-Haskell > mostraPessoa (Pessoa “Éderson Araújo” 22)
Nome: Éderson Araújo Idade: 22-}

data Forma = Circulo Float | Retangulo Float Float

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a

data Rua = Numero Int Residencia | Nome String Residencia
data Residencia = Casa Int | Apartamento Int Int
data Meses = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez
   deriving (Eq, Show, Enum)

{-Haskell > Jan
Jan
Haskell > Mar == Mar
True
Haskell > [Jan .. Set]
[Jan,Fev,Mar,Abr,Mai,Jun,Jul,Ago,Set]
-}
---------------------------------

--INTERACAO COM O USUARIO

main = do
  putStr (“Escreva uma palavra: “)
  palavra <- getLine
  putStr (“Palavra invertida: “++ reverse palavra)

---------------------------------

--
