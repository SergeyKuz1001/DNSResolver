{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Utils (
    (?:),
    (?^),
    (<$?>),
    tryForAll,
  ) where

import Data.Maybe (mapMaybe)
import Control.Applicative (Alternative, (<|>), empty)

infix  2 ?:, ?^
infixl 4 <$?>

class Eitherable l r e | e -> l r where
    toEither :: e -> Either l r

instance Eitherable () a (Maybe a) where
    toEither Nothing  = Left ()
    toEither (Just x) = Right x

instance Eitherable e a (Either e a) where
    toEither = id

(?:) :: (Applicative f, Eitherable l r e) => e -> (l -> f r) -> f r
(?:) e lfr =
    case toEither e of
        Left  l -> lfr  l
        Right r -> pure r

(?^) :: (Monad m, Eitherable l r e) => m e -> (l -> m r) -> m r
(?^) me lmr = do
    e <- me
    e ?: lmr

(<$?>) :: (a -> Maybe b) -> [a] -> [b]
(<$?>) = mapMaybe

tryForAll :: (Alternative f, Foldable t, Functor t) => t a -> (a -> f b) -> f b
tryForAll = (foldr (<|>) empty .) . flip fmap
