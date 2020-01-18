module Course1_step3_3 where

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream) 

repeatHelper = id

data Odd = Odd Integer deriving (Eq,Show)
instance Enum Odd where
  succ (Odd x) = Odd (x + 2) 
  pred (Odd x) = Odd (x - 2)
  enumFrom (Odd a) = map Odd [a, a + 2..]
  enumFromThen (Odd a) (Odd b) = map Odd [a,b..]
  enumFromTo (Odd a) (Odd c) = map Odd [a, a+2..c]
  enumFromThenTo (Odd a) (Odd b) (Odd c) = map Odd [a,b..c]
  toEnum x = Odd $ toInteger x
  fromEnum (Odd x) = fromInteger x

change :: (Ord a, Num a) => [a] -> a -> [[a]]
change coins 0 = [[]]
change coins s = [coin:ch | coin <- coins, coin <= s, ch <- (change coins $ s - coin)]

