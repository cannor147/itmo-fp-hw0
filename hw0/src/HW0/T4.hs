module HW0.T4
  ( repeat'
  , map'
  , fib
  , fac
  ) where

import Data.Function (fix)
import GHC.Natural (Natural, naturalToInt)

repeat' :: a -> [a]
repeat' = fix . (:)

map' :: (a -> b) -> [a] -> [b]
map' = fix $ \rec f array ->
  case array of
    [] -> []
    x : xs -> (:) (f x) (rec f xs)

fib :: Natural -> Natural
fib = head . (fix $ \rec n -> if n <= 1 then drop (1 - n) [1, 0] else (\prev -> (sum $ take 2 prev) : prev) $ rec (n - 1)) . naturalToInt

fac :: Natural -> Natural
fac = fix $ \rec n -> if n == 0 then 1 else n * rec (n - 1)
