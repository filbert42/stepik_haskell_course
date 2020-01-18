module Course1_step4_5 where

data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x rest) = x : (fromList rest)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = (Cons x (toList xs))

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = (Suc (toNat (n-1)))

add :: Nat -> Nat -> Nat
add a b = toNat $ (fromNat a) + (fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = toNat $ (fromNat a) * (fromNat b)

fac :: Nat -> Nat
fac n = toNat $ product [1..(fromNat n)]

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node ltree rtree) = 1 + max (height ltree) (height rtree)

size :: Tree a -> Int
size (Leaf a) = 1
size (Node ltree rtree) = 1 + size ltree + size rtree

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf i) = (1, i)
    go (Node ltree rtree) = (0 + (fst $ go ltree) + (fst $ go rtree),
                             0 + (snd $ go ltree) + (snd $ go rtree))

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand (expand e1 :*: expand e) :+: expand (expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) = expand (expand e :*: expand e1) :+: expand (expand e :*: expand e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = if isSumProd res then res else expand res where
  res = expand e1 :*: expand e2
expand e = e

isSumProd :: Expr -> Bool
isSumProd (Val i) = True
isSumProd (Val i1 :*: Val i2) = True
isSumProd (e :*: (e1 :+: e2)) = False
isSumProd ((e1 :+: e2) :*: e) = False
isSumProd (e :+: e1) = (isSumProd e) && (isSumProd e1)
isSumProd (e :*: e1) = (isSumProd e) && (isSumProd e1)
