{-# LANGUAGE TemplateHaskell #-}
module TestRose(runTests) where
import Test.QuickCheck

import Golf.Rose

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

prop_rose_list :: [Int] -> Property
prop_rose_list xs = (not (null xs)) ==> toList (fromList xs) === xs

return []

runTests = $(quickCheckAll)
