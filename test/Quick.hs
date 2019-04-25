{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

import qualified TestList
import qualified TestLeafy
import qualified TestTree
import qualified TestRose

main = do
  putStrLn ""
  TestList.runTests
  TestLeafy.runTests
  TestTree.runTests
  TestRose.runTests
