module Golf.Leafy where

data Leafy a = Leaf a
             | Branch (Leafy a) (Leafy a)
             deriving(Eq, Show)
type L = Leafy

leaf = Leaf

fromList :: [a] -> Leafy a
fromList [x] = leaf x
fromList xs = Branch (fromList l) (fromList r) where
  (l, r) = splitAt half xs
  half = length xs `div` 2

instance Functor Leafy where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)


-- Required:
-- join . fmap leaf = join . leaf = id
-- join . fmap join = join

join :: L(L a) -> L a
join (Leaf t) = t
join (Branch l r) = Branch (join l) (join r)
