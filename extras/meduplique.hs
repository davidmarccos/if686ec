--DECLARANDO FUNCOES

idade :: Int -> Int
idade i = i



--funcoes n podem comecar com letras maiusculas
meDuplique x = x + x
nosDupliqueesome x y = x*2 + y*2
--posso  chamar funcoes dentro de funcoes
nosdupliqueesomeoutraforma x y = meDuplique x + meDuplique y
--o if tem quer ter obrigatoriamente o else
duplicoseformenorquecem x = if x > 10 then x else x*2


--LISTAS

--strings tambem sao listas de caracteres, logo posso usar funcoes
--de listas em strings
lista2 = "hello" ++ " " ++ "word" 
--colocar duas listas juntas tenho que usar o operador ++
--usar listas direto no ghci tenho que usar a palavra chave let antes da lista
lista = [1,2,3,4] ++ [9,10,11,12]

--Se você deseja obter um elemento de uma lista pelo seu índice, utilize !!. O índice inicia a partir de 0
lista3 = "david marcos" !! 4 --aqui peguei a letra d
lista4 = [1,2,45,7,9] !! 2 --aqui peguei o 45

--posso colocar alguma coisa no inicio da lista usando o contra operador :
lista5 = 'q' : " gatinha"
lista6 = 56:[4,8,9,71,12] --se quiser adicionar no fim da lista eh so fazer ao contrario

--adicionando listas em listas
lista7 = [[1,2,3]] --criei uma lista de lista
lista8 = lista7 ++ [[4,7,6]] --adicionei ao final da lista7
lista9 = [4,2,1]:lista8 -- adicionaei uma lista no inicio da lista8
lista10 = lista9 !! 2 --aqui peguei a terceira lista da lista de listas 9
--as listas dentro de uma lista pode ter tamanhos diferentes mas não tipos diferentes

--fazendo comparacao de listas com == < > <= >= em ondem lexicográfica comparando primeiro pelo cabecalho
comparalista = [1,2,3] > [2,3,5] --o resultado eh um valor booleano

--outras funcoes de listas

--head (cabeça) mostra o primeiro elemento da lista
--no ghci head [1,2,3] resultado eh 1
--tail mostra a lista sem o valor da cabeca head
--last retorna o ultimo elemento da lista
--init retorna a lista sem o ultimo elemento
--length retorna o tamanho da lista
--null verifica se a lista eh vazia, se for retorna true
--reverse reverte uma lista onde o ultimo elemento agora será o primeiro e etc
--take recebe um numero e uma lista, e extrai a quantidade de elementos desde o inicio da lista
--se for uma lista de 10 elementos e eu fizer take 2 [1,...,10] retornará a lista com os dois primeiros elementos
--drop funciona como o take mas ele pega os numeros do final da lista
--maximum retorna o maior valor da lista
--minimum retorna o menor
--sum recebe uma lista de numeros e retorna sua soma
--product recebe uma lista de numeros e retorna seu produto
--elem recebe uma coisa e verifica se essa coisa esta na lista
--no ghci faz 4 `elem` [3,4,5,6] retornara true

--obter uma lista de 1 a 20
lista11 = [1..20]
--lista de a a z ou de k a z
lista12 = ['a'..'z']
lista13 = ['K'..'Z']
--lista de 2 em 2 de até 20
lista14 = [2,4..20]
lista15 = [0.1,0.3 .. 1]

