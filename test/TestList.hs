{-# LANGUAGE TemplateHaskell #-}
module TestList(runTests) where
import Test.QuickCheck

import Golf.List(prod,prodWith)

instance Show (a->b) where
  show f = "<function>"

prop_prod_dist xs ys zs = prod (xs ++ ys) zs === prod xs zs ++ prod ys zs
  where types = [xs,ys,zs::[Int]]

prop_prodWith_dist f xs ys zs = prodWith f (xs ++ ys) zs == prodWith f xs zs ++ prodWith f ys zs
  where types = (f::Int->Int->Int,[xs,ys,zs::[Int]])

return []

runTests = $(quickCheckAll)
