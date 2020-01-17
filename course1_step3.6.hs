lastElem :: [a] -> a
lastElem = foldl1 (\x y -> y)

revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr g b
  where g x = if (x < a) || (x > b) then Nothing else (Just (x, pred x))