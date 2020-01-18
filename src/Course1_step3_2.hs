module Course1_step3_2 where

import Data.Char
readDigits :: String -> (String, String)
readDigits xs = (takeWhile isDigit xs, dropWhile isDigit xs)

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 xs@(x:xs')
  | p1 x || p2 x = x : filterDisj p1 p2 xs'
  | otherwise = filterDisj p1 p2 xs'

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort list@(x:xs) =
  let
    bigger  = filter (> x) list
    equal   = filter (== x) list
    smaller = filter (< x) list
  in
    qsort smaller ++ equal ++ qsort bigger


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

perms :: [a] -> [[a]]
perms = foldr (concatMap . insert) [[]]
  where insert :: a -> [a] -> [[a]]
        insert x [] = [[x]]
        insert x all@(y:ys) = (x:all) : map (y:) (insert x ys)

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = \x y z -> zipWith max (zipWith max x y) z