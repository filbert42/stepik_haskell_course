concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\x s -> s + 1) 0 

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if x `mod` 2 == 1 then x + s else s) 0