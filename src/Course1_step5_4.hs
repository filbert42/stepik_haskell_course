module Course1_step5_4 where

import Data.Char ( isDigit )

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken x
  | all (== True) $ map isDigit x = Just $ Number (read x)
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = sequence [asToken x | x <- (words input)]

data Board = Board Integer deriving Show

oddBoard :: Board -> Bool
oddBoard (Board b) = odd b

nextPositions (Board x) = map Board [x-1,x+1] 

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
    | n < 0 = []
    | n == 0 = if pred b == True then [b] else []
    | otherwise = do
        possible_next_states <- nextPositions b
        result <- nextPositionsN possible_next_states (n-1) pred
        return result

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = 
    do 
        True <- return (x > 0)
        a <- [1..x]
        b <- [1..x]
        c <- [1..x]
        True <- return (b > a)
        True <- return (a^2 + b^2 == c^2)
        return (a, b, c)
