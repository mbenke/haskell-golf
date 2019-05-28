{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Golf.Vec where
import Golf.Nat

infixr 6 :>
data Vec a (n :: Nat) where
  V0  :: Vec a Z
  (:>) :: a -> Vec a n -> Vec a (S n)

vhead :: Vec a (S n) -> a
vhead (x:>_) = x

vtail :: Vec a (S n) -> Vec a n
vtail (_:>xs) = xs
