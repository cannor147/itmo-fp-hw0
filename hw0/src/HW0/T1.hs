{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->)(..)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

-- | Custom implementation of bijection.
data a <-> b = Iso (a -> b) (b -> a)

-- | Flips bijection.
flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

-- | Converts bijection to usual function.
runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

-- | Distributive law for Either.
distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left x)       = (Left x, Left x)
distrib (Right (y, z)) = (Right y, Right z)

-- | Associative law for Tuple (bidirectionally).
assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso leftAssocPair rightAssocPair
  where
    leftAssocPair  p = ((fst p, fst $ snd p), snd $ snd p)
    rightAssocPair p = (fst $ fst p, (snd $ fst p, snd p))

-- | Associative law for Either (bidirectionally).
assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso leftAssocEither rightAssocEither
  where
    leftAssocEither  (Left x)          = Left $ Left x
    leftAssocEither  (Right (Left x))  = Left $ Right x
    leftAssocEither  (Right (Right x)) = Right x
    rightAssocEither (Left (Left x))   = Left x
    rightAssocEither (Left (Right x))  = Right $ Left x
    rightAssocEither (Right x)         = Right $ Right x
