{-# LANGUAGE TupleSections #-}
module Golf.List where


prod :: [a] -> [b] -> [(a,b)]
-- prod xs ys = [(x,y) | x <- xs, y <- ys]
prod (x:xs) ys = map (x,) ys ++ prod xs ys
prod _ _ = []
