{-Exercícios Práticos 3

1 - Considere as seguintes funções:
f1 :: (a -> b) -> [a] -> [b]
f2 :: (a -> b -> a) -> a -> [b] -> a
f3 :: (a -> b -> a) -> a -> [b] -> a
f4:: (a -> bool) -> [a] -> ([a],[a])
(+) :: (Num a) => a -> a -> a
(.) :: (b -> c) -> (a -> b) -> a -> c
Determine o tipo, e comente o comportamento, de cada uma das funções
abaixo, mostrando os passos até obter o resultado. Se for preciso, identifique as
classes dos parâmetros polimórficos. Caso não seja possível determinar o tipo,
explique o porquê.
a) f3.f4
c) f1.f2 (+)
e) f4.f1.f1.f2

2 - Defina um tipo algébrico Temperatura com 3 construtores (Celsius, Fahrenheit e
Kelvin) que terão valores (Float) representando temperaturas nas escalas indicadas.
Em seguida, crie instâncias das classes Ord, Eq e Show para Temperatura (leve em
conta que TemperaturaCelsius/5 = (TemperaturaFahrenheit-32)/9 =
(TemperaturaKelvin-273)/5) e depois crie uma função minMax, que recebe um lista
de temperaturas e retorna um par em que o primeiro elemento é a menor
temperatura da lista e o segundo elemento a maior.
-}
data Temperature = Celsius Float | Fahrenheit Float | Kelvin Float   

showTmp :: Temperature -> String
showTmp (Celsius x)      = "Temp: " ++ show x ++ "º C"
showTmp (Fahrenheit x)   = "Temp: " ++ show x ++ "° F"
showTmp (Kelvin x)       = "Temp: " ++ show x ++ " K"

toKelvin :: Temperature -> Float
toKelvin (Celsius x)        = (x + 273)
toKelvin (Fahrenheit x)     = ((((x - 32) / 9) * 5) + 273)
toKelvin (Kelvin x)         = x

cmpTmp :: (Float -> Float -> Bool) -> Temperature -> Temperature -> Bool
cmpTmp f (Kelvin x) (Kelvin y)             = f x y
cmpTmp f (Celsius x) (Celsius y)           = f x y
cmpTmp f (Fahrenheit x) (Fahrenheit y)     = f x y 
cmpTmp f (Kelvin x) (Celsius y)            = f (toKelvin (Kelvin x)) (toKelvin (Celsius y))
cmpTmp f (Kelvin x) (Fahrenheit y)         = f (toKelvin (Kelvin x)) (toKelvin (Fahrenheit y))
cmpTmp f (Celsius x) (Fahrenheit y)        = f (toKelvin (Celsius x)) (toKelvin (Fahrenheit y))
cmpTmp f (Celsius y) (Kelvin x)            = cmpTmp f (Celsius y) (Kelvin x)
cmpTmp f (Fahrenheit y) (Kelvin x)         = cmpTmp f (Fahrenheit y) (Kelvin x)
cmpTmp f (Fahrenheit y) (Celsius x)        = cmpTmp f (Fahrenheit y) (Celsius x)

instance Show Temperature where
        show = showTmp

instance Eq Temperature where
        (==) = (eqTmp (==))

instance Ord Temperature where 
        (<=)  = (cmpTmp (<=))
        (>) x y = (cmpTmp (>)) y x

minMax :: [Temperature] -> (Temperature, Temperature)
minMax [] = error "There\'s no min or max to a Empty list."
minMax (a:as) = (min (a:as), max (a:as))

--minMax :: [Temperature] -> (Temperature, Temperature)
--minMax [] = error "There\'s no min or max to a Empty list."
--minMax (a:as) =  (minTmp (a:as), maxTmp (a:as))
--    where minTmp (a:as) | a <= minTmp as    = a
--                        | otherwise         = minTmp as    
--          minTmp (a:[])                     = a
--          maxTmp (a:as) | a >= maxTmp as    = a
--                        | otherwise         = maxTmp as
--          maxTmp (a:[])                     = a

{-
3 - Dado, abaixo, os tipos algébricos polimórficos LQueue e RQueue que
representam filas, crie uma classe OprQueue que contenha as funções enqueue,
dequeue, peek e isEmpty.
data LQueue t = LQ [t] deriving (Show)
data RQueue t = Empty | RQ t (RQueue t) deriving (Show)
Crie também instâncias da classe OprQueue para os dois tipos algébricos acima.
Caso a fila esteja vazia e seja utilizada alguma das operações dequeue ou peek,
encerre a execução do programa e exiba a mensagem de erro: “Empty Queue!”.
Exemplos:
Main> peek (dequeue (enqueue (LQ [3, 4, 5]) 7))
4
Main> isEmpty (enqueue (enqueue Empty (>5)) (==0))
False

Main> enqueue (LQ []) (RQ [12.96, 0.1225] Empty)
LQ [RQ [12.96, 0.1225] Empty]
-}

--- Q1
{-- A) A função f3 espera como entrada uma função e mais 2 argumentos, dos quais o tipo do primeiro 
--  argumento e da saída devem ser iguais, o que não acontece com a função f4, que recebe também 
--  uma função, mas recebe apenas uma lista de um tipo e retorna um par de duas listas.
--  Para que esta concatenação desse certo, deveriamos utilizar f4 do tipo [a] -> b -> [a] --}

{-- B) Primeiramente, a função (+) servirá como primeiro parâmetro para f2, tornando o tipo
    de f2 (+) :: a -> [a] -> a (lembrando que tipo b = tipo a nesse caso pois (+) :: a -> a -> a).
    Portanto, esta função será recebida como parâmetro para f1 
--}

f1 :: (a -> b) -> [a] -> [b]
f1 f a = map f a
f2 :: (a -> b -> a) -> a -> [b] -> a
f2 f a b = a

f3 :: (a -> b -> a) -> a -> [b] -> a
f3 f a b = a
f4 :: (a -> bool) -> [a] -> ([a],[a])
f4 f a = (a,a)

--- Q2
data Temperatura = Celsius Float | Fahrenheit Float | Kelvin Float
instance Show Temperatura where
    show (Celsius n)= show(n) ++ " C"
    show (Fahrenheit n) = show(n) ++ " F"
    show (Kelvin n) = show(n) ++ " K"

instance Eq Temperatura where
    (==) (Celsius c) (Fahrenheit f) = c == (5*(f-32)/9)
    (==) (Fahrenheit f) (Celsius c) = Celsius c == Fahrenheit f 
    (==) (Celsius c) (Kelvin k) = c == (k-273)
    (==) (Kelvin k) (Celsius c) = Celsius c == Kelvin k
    (==) (Fahrenheit f) (Kelvin k) = ((f-32)/9) == ((k-273)/5)
    (==) (Kelvin k) (Fahrenheit f) = Fahrenheit f == Kelvin k

instance Ord Temperatura where
    (compare) (Celsius c) (Fahrenheit f) = c `compare` (5*(f-32)/9)
    (compare) (Fahrenheit f) (Celsius c) = (5*(f-32)/9) `compare` c
    (compare) (Celsius c) (Kelvin k) = c `compare` (k-273)
    (compare) (Kelvin k) (Celsius c) = (k-273)  `compare`  c
    (compare) (Fahrenheit f) (Kelvin k) = ((f-32)/9) `compare` ((k-273)/5)
    (compare) (Kelvin k) (Fahrenheit f) = ((k-273)/5) `compare` ((f-32)/9)

minMax :: [Temperatura] -> (Temperatura,Temperatura)
minMax temps = (getMin temps (head temps), getMax temps (head temps))
    where 
        getMin [] res = res
        getMin (a:as) x | a < x = getMin as a
                        | otherwise = getMin as x
        
        getMax [] res = res
        getMax (a:as) x | a > x = getMax as a
                        | otherwise = getMax as x
-- Q3
data LQueue t = LQ [t] deriving (Show)
data RQueue t = Empty | RQ t (RQueue t) deriving (Show)