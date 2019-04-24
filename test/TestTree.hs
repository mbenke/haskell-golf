{-# LANGUAGE TemplateHaskell #-}
module TestTree(runTests) where
import Test.QuickCheck

import Golf.Tree as Tree

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree
  shrink Empty = []
  shrink (Node x l r) = [Empty,l,r] ++ [Node x' l' r' | (x',l',r') <- shrink (x,l,r)]

arbTree 0 = pure Empty
arbTree n = frequency
  [ (1, pure Empty)
  , (4, Node <$> arbitrary <*> arbTree m <*> arbTree m)
  ] where m = div n 2


prop_tree_list :: [Int] -> Property
prop_tree_list xs = toList (fromList xs) === xs

prop_tree_merge1 t = mempty <> t === t
  where types = [t::Tree Int]

prop_tree_merge2 t = t <> mempty === t
  where types = [t::Tree Int]

prop_tree_merge3 t u v = (t <> u) <> v === (t <> u) <> v
  where types = [t,u,v::Tree Int]

prop_tree_merge3b t u v = (t <> u) <> v === (t <> u) <> v
  where types = [t,u,v::Tree (Tree Bool)]

-- join . leaf = id
prop_tree_join1 t = join (leaf t) === t
  where types = (t::Tree(Tree(Int)))

-- join . fmap leaf = id
prop_tree_join2 t = join (fmap leaf t) === t
  where types = (t::Tree(Int))

prop_tree_join2b t = join (fmap leaf t) === t
  where types = (t::Tree(Tree(Int)))

-- join . fmap join = join . join
prop_tree_join3 t = join(fmap join t) === join(join t)
   where types = (t::Tree(Tree(Tree(Int))))

prop_getLast xs = let t = fromList xs in case reverse xs::[Int] of
  [] -> getLast t === Nothing
  (y:ys) -> case getLast t of
    Nothing -> Nothing === Just y
    Just (_, x) -> y === x

-- ------------ --
return []

runTests = $(quickCheckAll)
