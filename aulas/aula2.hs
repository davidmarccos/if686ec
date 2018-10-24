myNot :: Bool -> Bool
myNot True = False
myNot False = True
myOr :: Bool -> Bool -> Bool
myOr True x = True
myOr False x = x
myAnd :: Bool -> Bool -> Bool
myAnd False x = False
myAnd True x = x
compara4igual :: Int -> Int -> Int -> Int -> Bool
compara4igual a b c d = (a==b) && (b==c) && (c==d)
quantosiguais :: Int -> Int -> Int -> Int
quantosiguais a b c = if a == b or a==c
                        then x = 1
                        else if 

sumSquares :: Int -> Int -> Int
sumSquares x y = sqX + sqY 
  where sqX = x * x
      sqY = y * y
sumSquares x y = sq x + sq y
  where sq z = z * z
sumSquares x y =  let sqX = x * x
                     sqY = y * y
                  in sqX + sqY