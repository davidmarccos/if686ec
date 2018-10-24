substr :: String -> String -> Bool
substr [] [_] = False
substr (x : xs) 