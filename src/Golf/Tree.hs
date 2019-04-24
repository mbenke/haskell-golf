{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Golf.Tree where
import Test.QuickCheck

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Functor)

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

-- Compare trees by their content
instance Eq a => Eq (Tree a) where
  t1 == t2 = toList t1 == toList t2

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

-- merge by promoting last leaf seems in fact worse
{-
merge l Empty = l
merge l r = case getLast l of
  Nothing -> r
  Just (l', x) -> Node x l' r
-}

instance Semigroup (Tree a) where
  (<>) = merge

instance Monoid (Tree a) where
  mempty = Empty



-- join . fmap pure = join . pure = id
-- join . fmap join = join
join :: Tree (Tree a) -> Tree a
-- This works if we compare trees by content
-- when comparing by structure, `join . fmap leaf = id` fails
join Empty = Empty
join (Node t l r) = t <> join l <> join r

-- Some more failed attempts:
-- This fails at both `join . fmap leaf = id` and `join .fmap join = join . join`
-- join (Node x Empty Empty) = x
-- join (Node t l r) = fromList (concatMap toList [join l, t, join r])

-- This is no better
-- join = fromList . toList .join' where
--   join' Empty = Empty
--   join' (Node t l r) = t <> join l <> join r

-- instance Applicative Tree where
--   pure = leaf
