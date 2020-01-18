module Course1_step4_1 where

data Color = Red | Green | Blue deriving (Show, Read)
-- instance Show Color where
--   show Red = "Red"
--   show Green = "Green"
--   show Blue = "Blue"

charToInt :: Char -> Int
charToInt a = read [a]

stringToColor :: String -> Color
stringToColor s
  | s `elem` ["Red", "Green", "Blue"] = read s

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Info Error    = LT
cmp Info Warning  = LT
cmp Warning Error = LT
cmp Warning Info  = GT
cmp Error Warning = GT
cmp Error Info    = GT
cmp _     _       = EQ


-- data Result = Fail | Success

-- processData :: SomeData -> String
-- processData d =
--   case doSomeWork d of
--     (Success, 0) -> "Success"
--     (Fail,    n) -> "Fail: "++ show n