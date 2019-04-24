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


instance Arbitrary a => Arbitrary (Rose a) where
  arbitrary = sized arbRose

arbLeaf :: Arbitrary a => Gen (Rose a)

arbLeaf = leaf <$> arbitrary
arbRose 0 = arbLeaf
arbRose n = frequency [(1, arbLeaf), (4, go)] where
  go = do
    m <- choose (0, div n 2)
    root <- arbitrary
    children <- sequence [arbRose  (div n m) | i <- [1..m]]
    return (Rose root children)
