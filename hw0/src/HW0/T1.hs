{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->)(..)
  , flipIso
  , runIso
  , distrib
  , assocPair
  , assocEither
  ) where

-- | Custom implementation of bijection.
data a <-> b = Iso (a -> b) (b -> a)

-- | Flipping bijection.
flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

-- | Conversion bijection to usual function.
runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

-- | Distributive law for Either.
distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left x)       = (Left x, Left x)
distrib (Right (y, z)) = (Right y, Right z)

-- | Associative law for Tuple (bidirectionally).
assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair =
  Iso
  (\x -> ((fst x, fst $ snd x), snd $ snd x))
  (\y -> (fst $ fst y, (snd $ fst y, snd y)))

-- | Associative law for Either (bidirectionally).
assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso leftAssocEither rightAssocEither
  where
    leftAssocEither :: Either a (Either b c) -> Either (Either a b) c
    leftAssocEither (Left x)          = Left $ Left x
    leftAssocEither (Right (Left x))  = Left $ Right x
    leftAssocEither (Right (Right x)) = Right x

    rightAssocEither :: Either (Either a b) c -> Either a (Either b c)
    rightAssocEither (Left (Left x))  = Left x
    rightAssocEither (Left (Right x)) = Right $ Left x
    rightAssocEither (Right x)        = Right $ Right x
