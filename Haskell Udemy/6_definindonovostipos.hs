type Nome = String
type Idade = Int
type Linguagem = String
type Pessoa = (Nome,Idade,Linguagem)

pessoa :: Pessoa
pessoa = ("joao",20,"Haskell")

--pegar apenas o nome da pessoa
nome :: Pessoa -> Nome
nome (n,i,l) = n