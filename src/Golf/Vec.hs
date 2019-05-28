{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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

vappend :: Vec a m -> Vec a n -> Vec a (m:+n)
vappend V0 ys = ys
vappend (x:>xs) ys = x:>vappend xs ys

atIndex :: Vec a n -> Fin n -> a
atIndex (x:>_) FinZ = x
atIndex (_:>xs) (FinS k) = atIndex xs k
