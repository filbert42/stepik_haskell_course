module Course1_step1_6 where

seqA :: Integer -> Integer
seqA n 
    | elem n [0,1,2] = n + 1
    | otherwise = 
    let 
        helper2 :: Integer -> Integer -> Integer -> Integer -> Integer
        helper2 acc3 acc2 acc1 2 = acc3
        helper2 acc3 acc2 acc1 num = 
            helper2 (acc3 + acc2 - (2 * acc1)) acc3 acc2 (num - 1)
    in
        helper2 3 2 1 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0,1)
sum'n'count x = (toInteger (sum (helper3 y)), (toInteger (length (helper3 y)))) where
    helper3 :: Integer -> [Integer]
    helper3 0 = []
    helper3 y = helper3 (y `div` 10) ++ [y `mod` 10]
    y = abs x

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b 
    | a == b = f a
    |otherwise = h * ((f a + f b)/2 + helperSum) where 
        helperSum = sum [f x | x <- [a + h,a + 2*h..b]]
        h = (b - a) / 1000000

