module Course1_step4_6 where

import Prelude hiding (lookup)
import qualified Data.List as L

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
    a <> b = Xor (a /= b)

instance Monoid Xor where
    mempty = Xor False

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup a => Semigroup (Maybe' a) where
    mempty <> (Maybe' Nothing) = Maybe' Nothing
    (Maybe' Nothing) <> mempty = Maybe' Nothing
    (Maybe' a) <> (Maybe' b) = Maybe' (a <> b)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
instance MapLike ListMap where
  empty = ListMap []
  lookup k (ListMap []) = Nothing
  lookup k (ListMap lmp)
    | k == (fst . head $ lmp) = Just (snd . head $ lmp)
    | otherwise = lookup k (ListMap (tail lmp))
  insert key value (ListMap lmp) =
    ListMap $ (filter (\(k, _) -> (k /= key)) lmp) ++ [(key, value)] 
  delete key (ListMap lmp) = ListMap $ filter (\(k, _) -> (k /= key)) lmp
  fromList lmp = ListMap lmp


-- class MapLike m where
--     empty :: m k v
--     lookup :: Ord k => k -> m k v -> Maybe v
--     insert :: Ord k => k -> v -> m k v -> m k v
--     delete :: Ord k => k -> m k v -> m k v
--     fromList :: Ord k => [(k,v)] -> m k v
--     fromList [] = empty
--     fromList ((k,v):xs) = insert k v (fromList xs)


newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty = ArrowMap (\x -> Nothing)
  lookup k (ArrowMap am_func) = am_func k
  insert k v (ArrowMap am_func) = ArrowMap (\x -> if x == k then Just v else am_func x)
  delete k (ArrowMap am_func) = ArrowMap (\x -> if x == k then Nothing else am_func x)
