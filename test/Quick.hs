{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

import qualified TestList
import qualified TestLeafy
import qualified TestTree

main = do
  putStrLn ""
  TestList.runTests
  TestLeafy.runTests
  TestTree.runTests
--   verboseCheck prop_tree_join2
{-
main = do
  writeln "\nprop_prod_dist"
  quickCheck prop_prod_dist
  quickCheck prop_prodWith_dist
  quickCheck prop_leafy_join1
  where
    writeln = putStrLn
-}
