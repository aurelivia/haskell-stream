{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Stream (
  -- ** Stream
  Stream(..)
  -- ** Constructors
  , fromSeq, toSeq
  ) where

import Prelude
import GHC.Exts (IsList(..), IsString(..))
import Data.Sequence (Seq(..), ViewR(..), (<|))
import qualified Data.Sequence as Seq
import Data.Bifunctor (first)

data Stream a = Done | forall g. Skip !(g -> Stream a) !g | forall g. Some a !(g -> Stream a) !g

--------------------------------------------------
-- Constructors
--------------------------------------------------

instance Semigroup (Stream a) where
  x <> Done           = x
  Done <> y           = y
  (Skip nx sg) <> y   = let !x' = nx sg in x' <> y
  (Some x nx sg) <> y = let !x' = nx sg in Some x (x' <>) y

instance Monoid (Stream a) where
  mempty = Done

instance Functor Stream where
  fmap _ Done           = Done
  fmap f (Skip nx sg)   = Skip (fmap f . nx) sg
  fmap f (Some x nx sg) = Some (f x) (fmap f . nx) sg

instance Applicative Stream where
  pure x = Some x id Done

  liftA2 _ Done _ = Done
  liftA2 _ _ Done = Done
  liftA2 f x y    = liftA2' f (x, y, y)

liftA2' :: (a -> b -> c) -> (Stream a, Stream b, Stream b) -> Stream c
liftA2' _ (Done, _, _)                        = Done
liftA2' f (Skip nx xg, Done, ys)              = let !x' = nx xg in Skip (liftA2' f) (x', Done, ys)
liftA2' f (Some _ nx xg, Done, ys)            = let !x' = nx xg in liftA2' f (x', ys, ys)
liftA2' f (x, Skip ny yg, ys)                 = let !y' = ny yg in Skip (liftA2' f) (x, y', ys)
liftA2' f (x'@(Some x _ _), Some y ny yg, ys) = let !fxy = f x y; !y' = ny yg in Some fxy (liftA2' f) (x', y', ys)

instance IsList (Stream a) where
  type Item (Stream a) = a

  fromList []     = Done
  fromList (x:xs) = Some x fromList xs

  toList Done           = []
  toList (Skip nx sg)   = toList (nx sg)
  toList (Some x nx sg) = x : toList (nx sg)

instance IsString (Stream Char) where
  fromString = fromList

fromSeq :: Seq a -> Stream a
fromSeq Seq.Empty = Done
fromSeq (x:<|xs)  = Some x fromSeq xs

toSeq :: Stream a -> Seq a
toSeq Done           = Seq.Empty
toSeq (Skip nx sg)   = toSeq (nx sg)
toSeq (Some x nx sg) = x <| toSeq (nx sg)

instance Show a => Show (Stream a) where
  {-# INLINE [0] showsPrec #-}
  showsPrec p = showsPrec p . toList

instance Read a => Read (Stream a) where
  {-# INLINE [0] readsPrec #-}
  readsPrec p = map (first fromList) . readsPrec p




--------------------------------------------------
-- Predicates
--------------------------------------------------

instance Eq a => Eq (Stream a) where
  Done == Done                     = True
  Done == _                        = False
  _ == Done                        = False
  (Skip nx xg) == y                = let !x' = nx xg in x' == y
  x == (Skip ny yg)                = let !y' = ny yg in x == y'
  (Some x nx xg) == (Some y ny yg) = (x == y) && let !x' = nx xg; !y' = ny yg in x' == y'