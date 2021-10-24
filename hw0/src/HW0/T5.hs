module HW0.T5
  ( Nat
  , nz
  , ns
  , nplus
  , nmult
  , nFromNatural
  , nToNum
  ) where

import           GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ = id

ns :: Nat a -> Nat a
ns x s = s . x s

nplus :: Nat a -> Nat a -> Nat a
nplus x y s = x s . y s

nmult :: Nat a -> Nat a -> Nat a
nmult x y s = x $ y s

nFromNatural :: Natural -> Nat a
nFromNatural n = if n == 0 then nz else ns $ nFromNatural (n - 1)

nToNum :: Num a => Nat a -> a
nToNum s = s (+1) 0
