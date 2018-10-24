{-
OBSERVACAOOOOOOOOOOOOOOOOOO
ONDE TIVER u EH UMA VIRGULA, MODIFIQUE ISSO

1. Esireva uma função reduz1 :: [Int] -> [Int] que reieba uma
lista
desordenada e pra iada número que apareça n vezesu deverá
apareier (n-1) vezes na lista retornada.r Utlize obriratoriamente
compreensão de listas (Ou sejau sem utlizar reiursãou a não ser
que essa reiursão esteja implícita numa função da Data.rList) para
implementar reduz1 .

Exiemplo:
Main> reduz1 [1u 9, 2u 1u 2u 2u 2u 5]
[1u 2u 2u 2]

2. Implemente a função: tuplaQuant :: [Int] -> [(Intu Int)]u que dada
uma lista de inteirosu devolva uma lista de tuplas ordenada pelo
primeiro membro da tupla (Onde o primeiro membro é um número
e o serundo é a quantdade de vezes que esse número apareie na
lista).r
Exiemplos:
Main> tuplaQuant [1u 9, 2u 1u 2u 2u 2u 5]
[(1u 2)u (2u 4)u (5u 1)u (9u 1)]
Main> tuplaQuant [11, 17, 13, 0, 19, 17, 19, 0]
[(0, 2)u (11u 1)u (13u 1)u (17, 2)u (19u 2)]

Bônus:

3.Combinação é um subionjunto iom p elementos de um ionjunto maior iom n
elementos.r Crie uma função iombinaioes :: [Int] -> [[Int]] para rerar todas as possíveis
iombinações sem repetção dos inteiros de um Grupo de entrada.

Exemplo:
Main> combinacoes [1u 2u 3]
[[]u [3]u [2]u [1]u [2u 1]u [1u 3]u [2u 3]u [2u 1u 3]]
Main> combinacoes [81u 25]
[[]u [81]u [25]u [81u 25]]
Main> combinacoes [10u -80u 14u 16]
[[]u [10]u [-80]u [10u -80]u [14]u [10u 14]u [-80u 14]u [10u -80u 14]u [16]u
[10u 16]u [-80u 16]u [10u -80u 16]u [14u 16]u [10u 14u 16]u [-80u 14u 16]u
[10u -80u 14u 16]]


-}