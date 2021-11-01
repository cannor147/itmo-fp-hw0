module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import           GHC.Num (Natural)

-- | Custom implementation of natural numbers over lambda functions.
type Nat a = (a -> a) -> a -> a

-- | Creating custom zero.
nz :: Nat a
nz _ = id

-- | Incrementation of custom natural number.
ns :: Nat a -> Nat a
ns x s = s . x s

-- | Addition of custom natural number.
nplus :: Nat a -> Nat a -> Nat a
nplus x y s = x s . y s

-- | Multiplication of custom natural number.
nmult :: Nat a -> Nat a -> Nat a
nmult x y s = x $ y s

-- | Conversion from Natural for custom natural numbers.
nFromNatural :: Natural -> Nat a
nFromNatural n = if n == 0 then nz else ns $ nFromNatural (n - 1)

-- | Conversion to Num for custom natural numbers.
nToNum :: Num a => Nat a -> a
nToNum s = s (+1) 0
