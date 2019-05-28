{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Golf.Nat where

data Nat = Z | S Nat

data SNat (n::Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- | `Fin n` - finite data type with `n` elements
data Fin (n::Nat) where
    FinZ :: Fin ('S n) -- zero is less than any successor
    FinS :: Fin n -> Fin ('S n) -- n is less than (n+1)

newtype Bump (p :: Nat -> *) (n :: Nat) = Bump { lower :: p (S n) }
-- Bump  :: p (S n) -> Bump p n
-- lower :: Bump p n -> p(S n)

recNat :: p Z -> (forall m. p m -> p (S m)) -> SNat n -> p n
recNat pZ pS SZ = pZ
recNat pZ pS (SS n) = pS (recNat pZ pS n)
