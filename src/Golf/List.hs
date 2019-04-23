{-# LANGUAGE TupleSections #-}
module Golf.List where


prod :: [a] -> [b] -> [(a,b)]
-- prod xs ys = [(x,y) | x <- xs, y <- ys]
-- prod (x:xs) ys = map (x,) ys ++ prod xs ys
-- prod _ _ = []
prod = prodWith (,)

prodWith :: (a->b->c) -> [a] -> [b] -> [c]
prodWith f (x:xs) ys = map (f x) ys ++ prodWith f xs ys
prodWith f [] _ = []
