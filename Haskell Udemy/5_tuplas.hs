func :: (Int, Int) -> (Int, Int) -> (Int, Int)
func (a,b) (c,d) = (a+c,c+d)

----

nomes :: (String,String,String)
nomes = ("David", "Pedro", "Ayrton")

mostra_primeiro (x,_,_) = x
mostra_segundo (_,y,_) = y
mostra_terceiro (_,_,z) = z

--no ghci faço mostra_primeiro nomes, e irá aparecer o primeiro nome da tupla