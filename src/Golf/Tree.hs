{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Golf.Tree where

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

ffor :: Functor f => f a -> (a->b) -> f b
ffor = flip fmap


-- join . fmap pure = join . pure = id
-- join . fmap join = join
join :: Tree (Tree a) -> Tree a
join Empty = Empty
-- join (Node t l r) =
