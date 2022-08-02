module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import           Data.Function ((&))
import           Data.Void     (Void)

-- | Custom implementation of negation.
type Not a = a -> Void

-- | Double negation (type is similar to `a -> ((a -> Void) -> Void))`.
doubleNeg :: a -> Not (Not a)
doubleNeg = (&)

-- | Reversed double negation.
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg = flip (.) doubleNeg
