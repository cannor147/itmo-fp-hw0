module HW0.T4
  ( repeat'
  , map'
  , fib
  , fac
  ) where

import           Data.Function (fix)
import           GHC.Natural   (Natural, naturalToInt)

-- | Turning an element into infinite List with Y combinator.
repeat' :: a -> [a]
repeat' = fix . (:)

-- | Mapping List with Y combinator.
map' :: (a -> b) -> [a] -> [b]
map' = fix $ \rec f array ->
  case array of
    []     -> []
    x : xs -> (:) (f x) (rec f xs)

-- | Calculation Fibonacci numbers with Y combinator.
fib :: Natural -> Natural
fib = head .
  fix (\rec n -> if n <= 1
    then drop (1 - n) [1, 0]
    else (\prev -> sum (take 2 prev) : prev) $ rec (n - 1)) . naturalToInt

-- | Calculation factorial with Y combinator.
fac :: Natural -> Natural
fac = fix $ \rec n -> if n == 0 then 1 else n * rec (n - 1)
