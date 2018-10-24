andd :: Bool -> Bool -> Bool
andd False _ = False
andd _ False = False
andd True True = True

orr :: Bool -> Bool -> Bool
orr False False = False
orr _ _ = True

-- _ se chama variável anonima