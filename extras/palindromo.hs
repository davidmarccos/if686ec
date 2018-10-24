

palindromo :: String -> Bool
palindromo str | reverse str == str = True
               | otherwise = False