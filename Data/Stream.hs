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
import Control.Applicative (Alternative(..))
import Data.Bifunctor (first)
import Data.Sequence (Seq(..), ViewR(..), (<|))
import qualified Data.Sequence as Seq
import GHC.Exts (IsList(..), IsString(..))

data Stream a = Done | forall g. Skip !(g -> Stream a) !g | forall g. Some a !(g -> Stream a) !g

instance Semigroup (Stream a) where
  x <> Done           = x
  Done <> y           = y
  (Skip nx xg) <> y   = let !x' = nx xg in x' <> y
  (Some x nx xg) <> y = let !x' = nx xg in Some x (x' <>) y

instance Monoid (Stream a) where
  mempty = Done

instance Functor Stream where
  fmap _ Done           = Done
  fmap f (Skip nx xg)   = let !x' = nx xg in fmap f x'
  fmap f (Some x nx xg) = Some (f x) (fmap f . nx) xg

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
liftA2' f (x'@(Some x _ _), Some y ny yg, ys) = let !y' = ny yg in Some (f x y) (liftA2' f) (x', y', ys)

instance Alternative Stream where
  empty = mempty
  (<|>) = (<>)

instance Monad Stream where
  Done >>= _           = Done
  (Skip nx xg) >>= f   = let !x' = nx xg in x' >>= f
  (Some x nx xg) >>= f = let !fx = f x; x' = nx xg in fx <> (x' >>= f)





--------------------------------------------------
-- Constructors
--------------------------------------------------


instance IsList (Stream a) where
  type Item (Stream a) = a

  fromList []     = Done
  fromList (x:xs) = Some x fromList xs

  toList Done           = []
  toList (Skip nx xg)   = toList (nx xg)
  toList (Some x nx xg) = x : toList (nx xg)

instance IsString (Stream Char) where
  fromString = fromList

fromSeq :: Seq a -> Stream a
fromSeq Seq.Empty = Done
fromSeq (x:<|xs)  = Some x fromSeq xs

toSeq :: Stream a -> Seq a
toSeq Done           = Seq.Empty
toSeq (Skip nx xg)   = toSeq (nx xg)
toSeq (Some x nx xg) = x <| toSeq (nx xg)

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