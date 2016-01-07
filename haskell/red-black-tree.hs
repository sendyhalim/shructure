module DataStructure where

data Color = Red | Black deriving (Show)
data RBTree a =
  Empty
  | RBTree Color (RBTree a) a (RBTree a)
    deriving (Show)

balance :: RBTree a -> RBTree a
balance (Empty) = Empty
balance (RBTree Black (RBTree Red c y (RBTree Red a x b)) z d) =
  RBTree Red (RBTree Black a x b) y (RBTree Black c z d)
balance (RBTree Black (RBTree Red (RBTree Red a x b) y c) z d) =
  RBTree Red (RBTree Black a x b) y (RBTree Black c z d)
balance (RBTree Black a x (RBTree Red (RBTree Red b y c) z d)) =
  RBTree Red (RBTree Black a x b) y (RBTree Black c z d)
balance (RBTree Black a x (RBTree Red b y (RBTree Red c z d))) =
  RBTree Red (RBTree Black a x b) y (RBTree Black c z d)
balance t = t

insertTo :: (Ord a) => RBTree a -> a -> RBTree a
insertTo (Empty) x = RBTree Red Empty x Empty
insertTo tree@(RBTree color left y right) x
  | x > y = balance (RBTree color left y (insertTo right x))
  | x < y = balance (RBTree color (insertTo left x) y right)
  | otherwise = tree

insert :: (Ord a) => RBTree a -> a -> RBTree a
insert Empty x = RBTree Red Empty x Empty
insert tree x = let (RBTree _ left y right) = insertTo tree x in RBTree Black left y right
