{-# LANGUAGE TemplateHaskell #-}
module TestLeafy(runTests) where
import Test.QuickCheck

import Golf.Leafy as Leafy

instance Arbitrary a => Arbitrary (Leafy a) where
  arbitrary = sized arbLeafy

arbLeafy 0 = Leaf <$> arbitrary
arbLeafy n = frequency
  [ (1, Leaf <$> arbitrary)
  , (4, Branch <$> arbLeafy m <*> arbLeafy m)
  ] where m = div n 2

-- join . leaf = id
prop_leafy_join1 :: Leafy(Leafy(Int)) -> Bool
prop_leafy_join1 t = join (leaf t) == t
  where types = (t::Leafy(Leafy(Int)))

-- join . fmap leaf = id
prop_leafy_join2 t = join (fmap leaf t) == t
  where types = (t::Leafy(Leafy(Int)))

-- join . fmap join = join . join
-- a trifle slow
prop_leafy_join3 t = Leafy.join(fmap join t) == join(join t)
   where types = (t::Leafy(Leafy(Leafy(Int))))

prop_leafy_list :: [Int] -> Property
prop_leafy_list xs = (not (null xs)) ==> toList (fromList xs) === xs

return []

runTests = $(quickCheckAll)
