module Course1_step4_4 where

import Data.Char(isDigit)
import Data.Foldable
import Data.List.Split 
import Text.Read

data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = sum $ helper x1 x2 ++ helper y1 y2 where
  helper x y = [abs $ y - x]

getCenter :: Double -> Coord Int -> Coord Double
getCenter a (Coord x y) = Coord ((fromIntegral x + 0.5)*a) ((fromIntegral y + 0.5)*a)

getCell :: Double -> Coord Double -> Coord Int
getCell a (Coord x y) = (Coord (floor $ x/a) (floor $ y/a))

findDigit :: [Char] -> Maybe Char
findDigit = find isDigit

findDigitOrX :: [Char] -> Char
findDigitOrX c = case findDigit c of
  Just a -> a
  Nothing -> 'X'

maybeToList :: Maybe a -> [a]
maybeToList m = case m of
  Nothing -> []
  Just x -> [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe l = Just $ head l

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson = parse_person_list . map (splitOn " = ") . lines where
  parse_person_list :: [[String]] -> Either Error Person
  parse_person_list list@(x:xs)
    | (and $ map (== 2) $ (map length) list) == False = Left ParsingError
    | ["firstName", s1]:["lastName", s2]:["age", s3]:(xs) <- list =
        if (readMaybe s3 :: Maybe Int) == Nothing then Left (IncorrectDataError s3) else
          Right (Person s1 s2 (read s3 :: Int))
    | otherwise = Left IncompleteDataError
  parse_person_list _ = Left ParsingError

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing
