module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import           Data.Char (isSpace)
import           HW0.T1    (distrib)

a :: (Either [Char] b, Either [Char] c)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))

b :: [Bool]
b = map isSpace "Hello, World"

c :: [Char]
c = if 1 > 0 || error "X" then "Y" else "Z"

-- | Function `a` in weak head normal form.
a_whnf :: (Either [Char] b, Either [Char] c)
a_whnf = (Left ("AB" ++ "CD" ++ "EF"), (Left ("AB" ++ "CD" ++ "EF")))

-- | Function `b` in weak head normal form.
b_whnf :: [Bool]
b_whnf = (isSpace 'H') : (map isSpace "ello, World")

-- | Function `c` in weak head normal form.
c_whnf :: [Char]
c_whnf = "Y"
