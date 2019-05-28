{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Golf.Vec where
import Golf.Nat
import Data.Proxy

infixr 6 :>
data Vec a (n :: Nat) where
  V0  :: Vec a Z
  (:>) :: a -> Vec a n -> Vec a (S n)

deriving instance Show a => Show (Vec a n)


vhead :: Vec a (S n) -> a
vhead (x:>_) = x

vtail :: Vec a (S n) -> Vec a n
vtail (_:>xs) = xs

{-
vappend :: Vec a m -> Vec a n -> Vec a (m:+n)
vappend V0 ys = ys
vappend (x:>xs) ys = x:>vappend xs ys
-}

atIndex :: Vec a n -> Fin n -> a
atIndex (x:>_) FinZ = x
atIndex (_:>xs) (FinS k) = atIndex xs k

vreplicate :: SNat n -> a -> Vec a n
vreplicate SZ _ = V0
vreplicate (SS n) x = x:>(vreplicate n x)

vchop :: SNat m -> Vec a (m:+n) -> (Vec a m, Vec a n)
vchop SZ xs = (V0, xs)
vchop (SS m) (x:>xs) = (x:>ys, zs) where
  (ys,zs) = vchop m xs

-- >>> let v = 1 :> (1 :> (1 :> V0)); two = SS(SS SZ) in vtake3 two Proxy v
vtakeP :: SNat m -> Proxy n -> Vec a (m :+ n) -> Vec a m
vtakeP SZ     _ _ = V0
vtakeP (SS m) n (x:>xs) = x :> vtakeP m n xs


-- Without Proxy, but needs AmbiguousTypes
-- >>> let v = 1 :> (1 :> (1 :> V0));  two = SS(SS SZ) in vtake two v
-- 1 :> (1 :> V0)
vtake :: forall n m a. SNat m -> Vec a (m :+ n) -> Vec a m
vtake SZ _ = V0
vtake (SS m) (x:>xs) = x :> vtake @n m xs

-- "bastard" foldr: induction on Nat, recursion on Vec
foldrVec :: forall (p::Nat-> *) a (m::Nat).
            (forall n. a -> p n -> p (S n)) -> p Z -> Vec a m -> p m
foldrVec f z V0 = z
foldrVec f z (a:>v) = f a (foldrVec f z v)


-- a more general bastard foldr with Proxy
foldrVecP :: forall (k::Nat)(p::Nat-> *) a (m::Nat).
            Proxy k -> (forall n. a -> p n -> p (S n)) -> p k -> Vec a m -> p (m:+k)
foldrVecP k f z V0 = z
foldrVecP k f z (a:>v) = f a (foldrVecP k f z v)

append :: Vec a m -> Vec a n -> Vec a (m:+n)
append xs ys = foldrVecP Proxy (:>) ys xs
