{-# LANGUAGE TupleSections #-}
module Golf.List where

-- | Product of two lists
-- >>> prod [0..1] [5..7]
-- [(0,5),(0,6),(0,7),(1,5),(1,6),(1,7)]

prod :: [a] -> [b] -> [(a,b)]

-- prod xs ys = [(x,y) | x <- xs, y <- ys]
-- prod (x:xs) ys = map (x,) ys ++ prod xs ys
-- prod _ _ = []
prod = prodWith (,)

-- | Generalised product of lists
-- >>> prodWith (*) [1,2] [3,4,5]
-- [3,4,5,6,8,10]

prodWith :: (a->b->c) -> [a] -> [b] -> [c]

-- prodWith f xs ys = [f x y | x <- xs, y <- ys]
prodWith f xs ys = concat (map g xs) where g x = for ys (f x)


for :: [a] -> (a->b) -> [b]
for = flip map
