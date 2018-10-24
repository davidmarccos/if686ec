fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

--USANDO GUARDAS
fib :: Int -> Int
fib x | (x == 0) = 0
      | (x == 1) = 1
      | otherwise = fib(x-1) + fib(x-2)