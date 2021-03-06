--
--Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 2/2017 - 10/10/2017
--
-- Nome: 
--

-- 1) (2.0) Escreva uma funcao locate, que recebe como entrada um elemento e uma lista de elementos, e retorna a localização (o índice) daquele elemento dentro da lista. 
-- A primeira posição na lista tem índice 0 (zero).
-- Caso o elemento não pertença à lista, deve ser retornado o valor (-1).
-- Exemplos: locate 'x' "abcdewxyz" ------>  6
--           locate 5   [5,98,7,32] ------>  0
--           locate True [False, False] --> -1
-- locate :: Eq t => t -> [t] -> Int
locate :: Eq t => t -> [t] -> Int
locate a as | (length list) > 0 = head list
            | otherwise = -1
      where
        list = [y| (x,y) <- (zip as [0..]), x == a]

-- 2) (3.0) Escreva uma função que verifique se uma lista está contida em outra (por exemplo, se uma String ésubstring de outra).
-- Exemplos: substr "abc" "xyz12abrt" ----> False
--           substr "abc" "aaabrsabcfr" --> True
--           substr "aab" "aacrtxxeaayb" -> False
-- substr :: String -> String -> Bool
substr :: String -> String -> Bool
substr at@(a:as) bt@(b:bs) | (locate a bt) /= -1 = find as bs || substr at bs
                           | otherwise = False
                        where
                          find _ [] = False
                          find [] _ = True
                          find (x:xs) (y:ys) | x==y = find xs ys
                                             | otherwise = False
-- 3) Um robô é controlado por 4 comandos: 
--    Left, para girar sua direção à esquerda 90 graus;
--    Right, para girar sua direção à direita em 90 graus;
--    Forward seguido de um número N, que indica um avanço de N metros.
--    Backward seguido de um número N, que indica um retrocesso de N metros.

-- Supondo que o robô comece na posição (0,0) (coordenadas) e direcionado para norte (i.e. para o posição (0,1)): 
-- (3.0) faça uma função destination que informe a localização do robô após uma sequêcia de comandos.

-- Exemplo de posições/coordenadas:
-- (-2, 2) (-1, 2) (0, 2) (1, 2) (2, 2)
-- (-2, 1) (-1, 1) (0, 1) (1, 1) (2, 1)
-- (-2, 0) (-1, 0) (0, 0) (1, 0) (2, 0)
-- (-2,-1) (-1,-1) (0,-1) (1,-1) (2,-1)
-- (-2,-2) (-1,-2) (0,-2) (1,-2) (2,-2)

data Command = Forward Int | Backward Int | TurnLeft |  TurnRight 
  deriving (Eq, Show)
data Direction = North | South | West | East

-- exemplo: destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> (0,1)
--          destination (0,0) [Backward 2, Forward 1] ---> (0,-1)
-- destination :: (Int,Int) -> [Command] -> (Int,Int)
destination :: (Int,Int) -> [Command] -> (Int,Int)
destination (a,b) (c:cs) = destination (sendCommand (a,b) c dir) cs
                          where 
                            sendCommand (x,y) (Forward n) dir = (x, y+n)
                            sendCommand (x,y) Backward n = (x, y+n)

-- exemplo: destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> (0,1)
--          destination (0,0) [Backward 2, Forward 1] ---> (0,-1)
-- destination :: (Int,Int) -> [Command] -> (Int,Int)

-- 4) (2.0) faça uma função faces que informe para qual direção o robô estará voltado ao final de uma sequência de comandos (North, South, East ou West), assumindo que ele começa voltado para a direção North.
-- exemplo: faces North [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> South
--          faces North [Backward 2, Forward 1] ---> North
--          faces North [TurnLeft, TurnLeft, TurnLeft] ---> East
-- faces ::  Direction -> [Command] -> Direction

