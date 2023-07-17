module Integer.NonzeroMagnitudeAffineTraversal where

import Essentials hiding (traverse)
import Integer (Integer)
import Integer.Natural (Natural)
import Integer.Positive (Positive)
import Integer.Positive qualified as Positive
import Integer.Signed (Signed (..))
import Integer.Signed qualified as Signed

class NonzeroMagnitudeAffineTraversal a where
  traverse :: Applicative f => (Positive -> f Positive) -> a -> f a
  preview :: a -> Maybe Positive

over :: NonzeroMagnitudeAffineTraversal a => (Positive -> Positive) -> a -> a
over f x = runIdentity (traverse (pure . f) x)

set :: NonzeroMagnitudeAffineTraversal a => Positive -> a -> a
set m x = over (\_ -> m) x

instance NonzeroMagnitudeAffineTraversal Positive where
  traverse f x = f x
  preview = Just

instance NonzeroMagnitudeAffineTraversal Natural where
  traverse f x = case Positive.fromNatural x of
    Nothing -> pure x
    Just p -> Positive.toNatural <$> f p
  preview = Positive.fromNatural

instance NonzeroMagnitudeAffineTraversal Signed where
  traverse f x = case x of
    Zero -> pure x
    Plus p -> Plus <$> traverse f p
    Minus p -> Minus <$> traverse f p
  preview x = case x of
    Zero -> Nothing
    NonZero _ p -> Just p

instance NonzeroMagnitudeAffineTraversal Integer where
  traverse f x =
    Signed.toInteger <$> traverse f (Signed.fromInteger x)
  preview x = preview (Signed.fromInteger x)
