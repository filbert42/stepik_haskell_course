module Course1_step5_1 where

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D a b c) = (Point3D (f a) (f b) (f c))

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
    fmap f (Point p) = (Point (fmap f p))
    fmap f (LineSegment p1 p2) = (LineSegment (fmap f p1) (fmap f p2))

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf Nothing) = (Leaf Nothing)
    fmap f (Leaf (Just a)) = (Leaf (fmap f (Just a)))
    fmap f (Branch t1 Nothing t2) = (Branch (fmap f t1) Nothing (fmap f t2))
    fmap f (Branch t1 (Just a) t2) = (Branch (fmap f t1) (fmap f (Just a)) (fmap f t2))

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = (Entry (k1, k2) (f v))

instance Functor (Map k1 k2) where
    fmap f (Map list_of_entries) = (Map $ map (fmap f) $ list_of_entries)

