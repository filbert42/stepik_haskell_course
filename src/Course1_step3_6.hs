module Course1_step3_6 where

import Data.List (unfoldr)

lastElem :: [a] -> a
lastElem = foldl1 (\x y -> y)

revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr g b
  where g x = if (x < a) || (x > b) then Nothing else (Just (x, pred x))