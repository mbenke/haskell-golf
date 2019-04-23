{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

import Golf.List(prod,prodWith)
import qualified Golf.Leafy as Leafy
import Golf.Leafy(Leafy(..))
import qualified Golf.Tree as Tree
import Golf.Tree(Tree)

instance Show (a->b) where
  show f = "<function>"

prop_prod_dist xs ys zs = prod (xs ++ ys) zs === prod xs zs ++ prod ys zs
  where types = [xs,ys,zs::[Int]]

prop_prodWith_dist f xs ys zs = prodWith f (xs ++ ys) zs == prodWith f xs zs ++ prodWith f ys zs
  where types = (f::Int->Int->Int,[xs,ys,zs::[Int]])

instance Arbitrary a => Arbitrary (Leafy a) where
  arbitrary = sized arbLeafy

arbLeafy 0 = Leaf <$> arbitrary
arbLeafy n = frequency
  [ (1, Leaf <$> arbitrary)
  , (4, Branch <$> arbLeafy m <*> arbLeafy m)
  ] where m = div n 2

-- join . leaf = id
prop_leafy_join1 :: Leafy(Leafy(Int)) -> Bool
prop_leafy_join1 t = Leafy.join (Leafy.leaf t) == t
  where types = (t::Leafy(Leafy(Int)))

-- join . fmap leaf = id
prop_leafy_join2 t = Leafy.join (fmap Leafy.leaf t) == t
  where types = (t::Leafy(Leafy(Int)))

-- join . fmap join = join . join
-- commented out as slow
-- prop_leafy_join3 t = Leafy.join(fmap Leafy.join t) == Leafy.join(Leafy.join t)
--   where types = (t::Leafy(Leafy(Leafy(Int))))

prop_leafy_list :: [Int] -> Property
prop_leafy_list xs = (not (null xs)) ==> Leafy.toList (Leafy.fromList xs) === xs


prop_tree_list :: [Int] -> Property
prop_tree_list xs = Tree.toList (Tree.fromList xs) === xs

prop_tree_merge1 t = mempty <> t === t
  where types = [t::Tree Int]

prop_tree_merge2 t = t <> mempty === t
  where types = [t::Tree Int]

prop_tree_merge3 t u v = (t <> u) <> v === (t <> u) <> v
  where types = [t,u,v::Tree Int]

return []

runTests = $(quickCheckAll)

main = do
  putStrLn ""
  runTests
  -- verboseCheck prop_leafy_list
{-
main = do
  writeln "\nprop_prod_dist"
  quickCheck prop_prod_dist
  quickCheck prop_prodWith_dist
  quickCheck prop_leafy_join1
  where
    writeln = putStrLn
-}
