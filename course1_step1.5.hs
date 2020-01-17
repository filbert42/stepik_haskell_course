doubleFact :: Integer -> Integer 
doubleFact n 
    | mod n 2 == 1 = product [1,3..n]
    | mod n 2 == 0 = product [2,4..n]

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (- 1) = 1
fibonacci n 
    | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
    | otherwise = fibonacci(n + 2) - fibonacci (n + 1)

fibonacci2 :: Integer -> Integer
fibonacci2 n = helper 0 1 n where
    helper acc1 acc2 n 
        | n > 1 = helper acc2 (acc1 + acc2) (n - 1)
        | n < (- 1) = helper acc2 (acc1 - acc2) (n + 1)
        | n == 0 = 0
        | otherwise = acc2
