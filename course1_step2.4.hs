class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
    
class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a
      | doesEnrageMork a && doesEnrageGork a = stomp (stab a)
      | doesEnrageMork a = stomp a
      | doesEnrageGork a = stab a
      | otherwise = a

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | x == maxBound = minBound
    | otherwise = succ x

  spred :: a -> a
  spred x
    | x == minBound = maxBound
    | otherwise = pred x

avg :: Int -> Int -> Int -> Double
avg a b c = (/ 3.0) $ (integer_division a 1.0 + integer_division b 1.0 + integer_division c 1.0) 
  
integer_division :: Int -> Double -> Double
integer_division = (/) . fromIntegral
