module Course1_step5_2 where

import Control.Monad (ap, liftM)

data Log a = Log [String] a
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg a  = Log [msg] (f a)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (msg1++msg2) (val2) where
  Log msg1 val1 = f x
  Log msg2 val2 = g val1

returnLog :: a -> Log a
returnLog a = Log [] a

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg val) f = (Log (msg++fmsg) fval) where
  Log fmsg fval = f val

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x list_of_funcs = foldl (>>=) (return x) list_of_funcs