module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import           Data.Function ((&))
import           Data.Void     (Void)

type Not a = a -> Void

-- doubleNeg :: a -> ((a -> Void) -> Void)
doubleNeg :: a -> Not (Not a)
doubleNeg = (&)

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg = flip (.) doubleNeg
