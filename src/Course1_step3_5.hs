module Course1_step3_5 where

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if x `mod` 2 == 1 then x + s else s) 0

evenOnly :: [a] -> [a]
evenOnly l = reverse . snd $ foldl f (0,[]) l where
  f (i, xs) x = if i `mod` 2 == 1 then (i+1, x:xs) else (i+1, xs)

evenOnly' :: [a] -> [a]
evenOnly' l = snd $ foldr f ([],[]) l where
  f x ~(xs, ys) = (x:ys, xs)
