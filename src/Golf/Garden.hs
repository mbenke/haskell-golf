module Golf.Garden where
import Test.QuickCheck

type Garden a = [Rose a]
data Rose a = Rose a [Rose a] deriving (Eq, Show)

leaf :: a -> Rose a
leaf x = Rose x []

fromList :: [a] -> Garden a
fromList xs = map leaf xs

toListR :: Rose a -> [a]
toListR (Rose x rs) = x:toList rs

toList :: Garden a -> [a]
toList rs = concatMap toListR rs
