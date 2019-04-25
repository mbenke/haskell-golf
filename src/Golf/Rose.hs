module Golf.Rose where
import Test.QuickCheck

data Rose a = Rose a [Rose a] deriving (Eq, Show)

leaf :: a -> Rose a
leaf x = Rose x []

fromList :: [a] -> Rose a
fromList [] = error "fromList: empty list"
fromList (x:xs) = Rose x (map  leaf xs)

toList :: Rose a -> [a]
toList (Rose x rs) = x:concatMap toList rs
