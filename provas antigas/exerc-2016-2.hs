--
--Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 2/2016 - 29/09/2016
--
-- Nome: 
--

{- O formato CSV (comma-spearated value) é bastante utilizado para disponbilizar dados abertos 
   ou para disponiblizar arquivos com logs.
   Abaixo temos um exemplo de um arquivo assim, onde a primeira linha é o título de cada coluna e as demais 
   são os valores de cada coluna separados por vírgulas.

   exemplo de arquivoi de log:
-------------------------------------
Day;Time;Event;Card;
2016-09-27;19:31:52;Normal;208772;
2016-09-27;18:12:02;Normal;155759;
2016-09-26;17:12:02;Normal;155759;
2016-09-26;16:11:02;Denied;188758;
2016-09-25;19:12:02;Normal;155759;
-------------------------------------

  Neste exemplo, de um log de controle de acesso de determinado mês, temos:
  um campo de data (ano-mês-dia) e hora (hora-minuto-segundo), 
  um campo com o tipo de evento de acesso (Normal ou Denied), 
  e o número de identificação do usuário.
  Nesse exemplo temos 5 eventos, em 3 dias (25, 26 e 27 de setembro), com 3 usuários diferentes.
-}

logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

{- Considerando que o arquivo está em uma String, e que os dados são de um mesmo mês,
   você deve escrever funções que dêem as seguintes informações:
-}

-- 1) (2.5) Quantos acessos foram permitidos e quantos foram negados no período? (isto é, na string toda).
-- o resultado é uma tupla, onde o primeiro valor é o total de acessos permitidos (Normal) e o segundo o de Negados (Denied)
-- exemplo:
-- tiposDeAcesso logSetembro -----> (4,1)
tiposDeAcesso :: String -> (Int, Int) 
tiposDeAcesso str = (length (filter (=='N') str), length (filter (== 'D') str))

-- 2) (2.5) Quantas tentativas de acessoxs ocorreram em cada dia? (considere que o arquivo já está ordenado por data e hora)
-- cada dia com acesso deve aparecer em uma lista de tuplas, o primeiro valor da tupla é o dia do mês e o segundo o número de acessos.
-- exemplo:
-- acessosPorDia logSetembro -----> [(27,2),(26,2),(25,1)] -- 2 acessos dia 27, 2 dia 26 e 1 dia 25
acessosPorDia :: String -> [(Int, Int)] 
acessosPorDia str = contaDias (filtraDias str)

filtraDias :: String -> [Int]
filtraDias [] = []
filtraDias str = strToInt (take 2 (drop 8 str)) : filtraDias (drop 35 str)

contaDias :: [Int] -> [(Int,Int)]
contaDias [] = []
contaDias (a:as) = (a,1 + length (takeWhile (== a) as)) : contaDias (dropWhile (== a) as)

-- 3) (2.5) Quantos acessos cada usuário realizou no período? 
-- exemplo:
-- acessosPorUsuario logSetembro -----> [(208772,1),(155759,3),(188758,1)] -- 2 acessos de um usuário e 1 de cada um dos outros
acessosPorUsuario :: String -> [(Int, Int)] 
acessosPorUsuario str = contaUsuarios (filtraUsuarios str)

filtraUsuarios :: String -> [Int]
filtraUsuarios [] = []
filtraUsuarios str = strToInt (take 6 (drop 27 str)) : filtraUsuarios (drop 35 str)

contaUsuarios :: [Int] -> [(Int,Int)]
contaUsuarios [] = []
contaUsuarios (a:as) = (a,1 + length (filter (== a) as)) : contaUsuarios (filter (/= a) as)

-- 4) (2.5) escreva a função converte, que transforma os dados armazenados na String para o seguinte tipo de dados:

type Dia = String
type Hora = String
type Usuario = String

data LogEntry = Permitido Dia Hora Usuario
              | Negado Dia Hora Usuario

     deriving Show

strToInt :: String -> Int
strToInt str = read str

converte :: String -> [LogEntry]
converte str = map convlogEntry (lines str)

breakItems :: String -> [String]
breakItems [] = []
breakItems str = takeWhile (/= ';') str : breakItems (tail (dropWhile (/= ';') str))

convlogEntry str = case item of
                     [day, time, "Normal", user] -> Permitido day time user
                     [day, time, "Denied", user] -> Negado day time user
  where item = breakItems str



