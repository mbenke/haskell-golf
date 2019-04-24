{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Golf.Tree where
import Test.QuickCheck

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Eq, Show, Functor)

leaf :: a -> Tree a
leaf a = Node a Empty Empty

fromList :: [a] -> Tree a
fromList [] = Empty
fromList (x:xs) = Node x (fromList l) (fromList r) where
  (l, r) = splitAt half xs
  half = length xs `div` 2

toListPrefix :: Tree a -> [a]
toListPrefix Empty = []
toListPrefix (Node x l r) = x : toListPrefix l ++ toListPrefix r

toList = toListPrefix


instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree
  shrink Empty = []
  shrink (Node x l r) = [Empty,l,r] ++ [Node x' l' r' | (x',l',r') <- shrink (x,l,r)]

arbTree 0 = pure Empty
arbTree n = frequency
  [ (1, pure Empty)
  , (4, Node <$> arbitrary <*> arbTree m <*> arbTree m)
  ] where m = div n 2

ffor :: Functor f => f a -> (a->b) -> f b
ffor = flip fmap

getLast :: Tree a -> Maybe (Tree a, a)
getLast Empty = Nothing
getLast (Node x l Empty) = Just (l, x)
getLast (Node x l r) = do
  (r', y) <- getLast r
  return (Node x l r', y)

merge :: Tree a -> Tree a -> Tree a
merge Empty t = t
merge (Node x l r) t = Node x l (merge r t)

instance Semigroup (Tree a) where
  (<>) = merge

instance Monoid (Tree a) where
  mempty = Empty



-- join . fmap pure = join . pure = id
-- join . fmap join = join
join :: Tree (Tree a) -> Tree a
join Empty = Empty
join (Node t l r) = join l <> t <> join r

-- instance Applicative Tree where
--   pure = leaf
