module Course1_step3_1 where
  
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y = (x :) . (y :)

nTimes:: a -> Int -> [a]
nTimes x 0 = []
nTimes x n = (x :) $! nTimes x (n-1)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = [] 
oddsOnly (x : xs) = if odd x then x : oddsOnly xs else oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

sum3 :: (Eq a, Num a) => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [0] [0] [0] = []
sum3 xs ys zs =
  let
    fixlist :: Num a => [a] -> [a]
    fixlist [] = [0]
    fixlist as = as
    headfix = head . fixlist
    fixtail = tail . fixlist
  in
    (headfix xs + headfix ys + headfix zs) : sum3 (fixtail xs) (fixtail ys) (fixtail zs)

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs) = 
  let
    helper :: Eq a => [a] -> ([a],[a])
    helper [] = ([],[])
    helper (x:xs) = span (== (head xs)) (x:xs)
    res = helper (x:xs)
  in
    if null (fst res) then [[x]] ++ groupElems xs
    else [fst res] ++ groupElems (snd res)