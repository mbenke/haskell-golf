{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

import qualified TestList
import qualified TestLeafy
import qualified TestTree
import qualified TestRose
import qualified TestGarden

main = do
  putStrLn ""
  TestList.runTests
  TestLeafy.runTests
  TestTree.runTests
  TestRose.runTests
  TestGarden.runTests
