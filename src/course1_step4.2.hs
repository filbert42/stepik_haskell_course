import Data.List

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 x2) (Point y1 y2) = sqrt $ (y1 - x1)^2 + (y2 - x2)^2

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle a b) = a*b

data Result' = Success' | Fail' Int

instance Show Result' where
    show Success' = "Success"
    show (Fail' err) = "Fail: " ++ show err

doSomeWork' :: SomeData -> Result'
doSomeWork' d = case doSomeWork d of
  (Success, 0) -> Success'
  (Fail,  err) -> (Fail' err)

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare (Circle _) = False

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

int_to_bit :: Int -> Z
int_to_bit 0 = (Z Plus [])
int_to_bit int = toz . map bit .  unfoldr f . abs $ int where
  f i = if i == 0 then Nothing else Just (i `mod` 2, i `div` 2)
  bit i = if i == 0 then Zero else One
  toz = if int >= 0 then Z Plus else Z Minus

bit_helper :: [Bit] -> Int
bit_helper = snd . foldr f (0,0) . map unbit . reverse where
  f elem (index, acc) = (index+1, acc + elem*(2^index))
  unbit Zero = 0
  unbit One = 1

bit_to_int:: Z -> Int
bit_to_int (Z Plus bit) = bit_helper bit
bit_to_int (Z Minus bit) = (-1) * (bit_helper bit)
  
add :: Z -> Z -> Z
add x y = int_to_bit $ bit_to_int x + bit_to_int y

mul :: Z -> Z -> Z
mul x y = int_to_bit $ bit_to_int x * bit_to_int y