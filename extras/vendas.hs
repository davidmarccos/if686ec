vendas :: Int -> Int
vendas 0 = 100
vendas 1 = 110
vendas 2 = 50
vendas 3 = 70
--vendas n = 0

totalVendas:: Int -> Int
totalVendas 0 - vendas 0
totalVendas n - vendas n + totalVendas(n-1)

maxiVendas :: Int -> Int
maxiVendas 0 - vendas 0
maxiVendas n - max (vendas n) (maxiVendas(n-1))

addespaços :: Int -> String
addespacos 0 = "" -- caso base gerando 0 espaços 
addespacos n = " " ++ addespacos (n-1) -- isso é uma recursao, eh meu loop em haskell, se for 100 espacos o addespacos fica 100-1= 99 e assim vai

--questao para direita
paraDireita :: Int -> String -> String
paraDireita n str = addespacos n ++ str --primeiro parametro eh n e segundo parametro e str, podendo mudar pra qualquer outra palavra sendo minuscula

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(cabecalho
					++ imprimeSemanas n
					++ imprimeTotal n
					++ imprimeMedia n
					)

cabecalho :: String
cabecalho = 		"Semana     Venda n"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = " 	0	  " ++ show(vendas 0) ++ "\n" --show converte pra string o vendas
imprimeSemanas n = imprimeSemanas (n-1) ++ paraDireita 5 (show n) ++ paraDireita 3 (show(vendas n)) ++ "\n"
-- imprimeSemanas (n-1) eh meu loop. coloquei no começo pra ficar na ordem da babela

imprimeTotal :: Int -> String
imprimeTotal n = "Total       " ++ paraDireita 14 show(totalVendas n) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "Media       " ++ paraDireita 14 show((totalVendas n) 'div' (n+1)) ++ "\n"


