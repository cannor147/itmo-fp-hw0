module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)
import Data.Function ((&))

type Not a = a -> Void

-- doubleNeg :: a -> ((a -> Void) -> Void)
doubleNeg :: a -> Not (Not a)
doubleNeg = (&)

-- can't prove
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg = undefined