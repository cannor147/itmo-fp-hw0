module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import           Data.Function (fix)
import           GHC.Natural   (Natural, naturalToInt)

-- | Turns an element into infinite List with Y combinator.
repeat' :: a -> [a]
repeat' = fix . (:)

-- | Maps List with Y combinator.
map' :: (a -> b) -> [a] -> [b]
map' = fix $ \rec f array ->
  case array of
    []     -> []
    x : xs -> (:) (f x) (rec f xs)

-- | Calculates Fibonacci numbers with Y combinator.
fib :: Natural -> Natural
fib = head . fibArr . naturalToInt
  where
    fibArr      = fix $ \rec n -> if n <= 1 then drop (1 - n) [1, 0] else append $ rec (n - 1)
    append prev = sum (take 2 prev) : prev

-- | Calculates factorial with Y combinator.
fac :: Natural -> Natural
fac = fix $ \rec n -> if n == 0 then 1 else n * rec (n - 1)
