module Course1_step5_7 where

import Control.Monad.Writer

type Shopping = Writer (Sum Integer) ()

evalWriter :: Writer w a -> a
evalWriter m = fst $ runWriter m

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), (Sum cost))

total :: Shopping -> Integer
total = getSum . execWriter