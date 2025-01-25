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
  , cons, snoc, fromSeq, toSeq, repeat, cycle
  -- ** Accessors
  , head, headElse, last, lastElse, uncons, (!?), length, done
  -- ** Slices
  , tail, init, take, drop, slice, takeWhile, dropWhile, span
  ) where

import Prelude hiding
  ( cycle, drop, dropWhile, head, init, last, length, repeat, span, tail, take, takeWhile
  )
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Data.Bifunctor (first)
import Data.Foldable (Foldable(foldr', foldl'))
import Data.Maybe (maybe, fromMaybe, isJust)
import Data.Sequence (Seq(..), ViewR(..), (<|))
import qualified Data.Sequence as Seq
import GHC.Exts (IsList(..), IsString(..))

data Stream a = Done | forall g. Skip !(g -> Stream a) g | forall g. Some a !(g -> Stream a) g

instance Semigroup (Stream a) where
  x <> Done           = x
  Done <> y           = y
  (Skip nx xg) <> y   = let x' = nx xg in Skip (x' <>) y
  (Some x nx xg) <> y = let x' = nx xg in Some x (x' <>) y

instance Monoid (Stream a) where
  mempty = Done

instance Functor Stream where
  fmap _ Done           = Done
  fmap f (Skip nx xg)   = Skip (fmap f . nx) xg
  fmap f (Some x nx xg) = Some (f x) (fmap f . nx) xg

instance Applicative Stream where
  pure x = Some x id Done

  liftA2 _ Done _ = Done
  liftA2 _ _ Done = Done
  liftA2 f x y    = liftA2' f (x, y, y) where
    liftA2' :: (a -> b -> c) -> (Stream a, Stream b, Stream b) -> Stream c
    liftA2' _ (Done, _, _)                        = Done
    liftA2' f (Skip nx xg, y, ys)                 = let x' = nx xg in Skip (liftA2' f) (x', y, ys)
    liftA2' f (Some _ nx xg, Done, ys)            = let x' = nx xg; !l = liftA2' f (x', ys, ys) in l
    liftA2' f (x, Skip ny yg, ys)                 = let y' = ny yg in Skip (liftA2' f) (x, y', ys)
    liftA2' f (x'@(Some x _ _), Some y ny yg, ys) = let y' = ny yg in Some (f x y) (liftA2' f) (x', y', ys)

instance Alternative Stream where
  empty = mempty
  (<|>) = (<>)

instance Monad Stream where
  Done >>= _           = Done
  (Skip nx xg) >>= f   = let x' = nx xg in Skip (>>= f) x'
  (Some x nx xg) >>= f = let fx = f x; x' = nx xg in fx <> (x' >>= f)

instance MonadPlus Stream

instance MonadFail Stream where
  fail _ = mempty





--------------------------------------------------
-- Constructors
--------------------------------------------------

cons :: a -> Stream a -> Stream a
cons x = Some x id

snoc :: Stream a -> a -> Stream a
snoc xs x = xs <> pure x

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

-- | Creates a stream consisting of a single element repeated infinitely.
repeat :: a -> Stream a
repeat x = Some x repeat x

-- | Turns a finite list into an infinite one by repeating it.
cycle :: Stream a -> Stream a
cycle Done = Done
cycle xs   = cycle' (xs, xs) where
  cycle' :: (Stream a, Stream a) -> Stream a
  cycle' (Done, xs)         = cycle' (xs, xs)
  cycle' (Skip nx xg, xs)   = let x' = nx xg in Skip cycle' (x', xs)
  cycle' (Some x nx xg, xs) = let x' = nx xg in Some x cycle' (x', xs)





--------------------------------------------------
-- Accessors
--------------------------------------------------

-- | \(\mathcal{O}(1)\) (Skips notwithstanding). Retrieve the first element available in a stream, or Nothing if there isn't one.
head :: Stream a -> Maybe a
head Done         = Nothing
head (Skip nx xg) = let !h = head (nx xg) in h
head (Some x _ _) = Just x

-- | \(\mathcal{O}(1)\). Retrieve the first element available in a stream, or a provided default if there isn't one.
headElse :: a -> Stream a -> a
headElse e = fromMaybe e . head

last :: Stream a -> Maybe a
last Done = Nothing
last (Skip nx xg) = let !l = last (nx xg) in l
last (Some x nx xg) = let !x' = nx xg in case x' of
  Done -> Just x
  _    -> let !l = last x' in l

lastElse :: a -> Stream a -> a
lastElse e = fromMaybe e . last

uncons :: Stream a -> Maybe (a, Stream a)
uncons Done           = Nothing
uncons (Skip nx xg)   = let !u = uncons (nx xg) in u
uncons (Some x nx xg) = let !x' = nx xg in Just (x, x')

-- | \(\mathcal{O}(n)\). Retrieve the \mathcal{n}th available element, or Nothing if none exists.
(!?) :: Stream a -> Int -> Maybe a
infixl 9 !?
Done !? _           = Nothing
(Skip nx xg) !? i   = let !n = nx xg !? i in n
(Some x nx xg) !? i
  | i < 0     = Nothing
  | i == 0    = Just x
  | otherwise = let !n = nx xg !? (i - 1) in n

length :: Stream a -> Int
length = foldl' (\l x -> x `seq` l + 1) 0

done :: Stream a -> Bool
done Done         = True
done (Skip nx xg) = let !d = done (nx xg) in d
done _            = False





--------------------------------------------------
-- Slices
--------------------------------------------------

-- | \(\mathcal{O}(1)\). Creates a new stream containing all elements of an existing stream except for first available.
tail :: Stream a -> Stream a
tail Done           = Done
tail (Skip nx xg)   = Skip tail (nx xg)
tail (Some _ nx xg) = let !x' = nx xg in x'

-- | \(\mathcal{O}(1)\). Creates a new stream containing all elements of an existing stream except the last available before it terminates. If it doesn't terminate then it's identical to the input.
init :: Stream a -> Stream a
init Done           = Done
init (Skip nx xg)   = Skip init (nx xg)
init (Some x nx xg) = let !x' = nx xg in case x' of
  Done -> Done
  _    -> Some x init x'

-- | \(\mathcal{O}()\). Produce a new stream consisting of only the first \mathcal{n} elements of a given stream.
take :: Int -> Stream a -> Stream a
take _ Done           = Done
take n (Skip nx xg)   = Skip (take n) (nx xg)
take n (Some x nx xg) = if n <= 0 then Done else Some x (take (n - 1)) (nx xg)

-- | \(\mathcal{O}(n)\). Produce a new stream consisting of all elements from a given stream except for the first \mathcal{n}.
drop :: Int -> Stream a -> Stream a
drop _ Done             = Done
drop n (Skip nx xg)     = Skip (drop n) (nx xg)
drop n x@(Some _ nx xg) = if n <= 0 then x else Skip (drop (n - 1)) (nx xg)

-- | \(\mathcal{O}(n)\). Produce a new stream consisting of the elements between \mathcal{s} and \mathcal{e}. Equivalent to @take (e - s) . drop s@
slice :: Int -> Int -> Stream a -> Stream a
slice s e = take (e - s) . drop s

takeWhile :: (a -> Bool) -> Stream a -> Stream a
takeWhile _ Done           = Done
takeWhile f (Skip nx xg)   = Skip (takeWhile f) (nx xg)
takeWhile f (Some x nx xg) = if f x then Some x (takeWhile f) (nx xg) else Done

dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile _ Done              = Done
dropWhile f (Skip nx xg)      = Skip (dropWhile f) (nx xg)
dropWhile f x'@(Some x nx xg) = if f x then Skip (dropWhile f) (nx xg) else x'

span :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
span f s = (takeWhile f s, dropWhile f s)





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





--------------------------------------------------
-- Folds
--------------------------------------------------

instance Foldable Stream where
  foldr _ z Done           = z
  foldr f z (Skip nx xg)   = let x' = nx xg in foldr f z x'
  foldr f z (Some x nx xg) = let x' = nx xg in f x $ foldr f z x'

  foldr' _ z Done           = z
  foldr' f z (Skip nx xg)   = let !fld = foldr' f z (nx xg) in fld
  foldr' f z (Some x nx xg) = let !fld = foldr' f z (nx xg); !fxz = f x fld in fxz

  foldl _ z Done           = z
  foldl f z (Skip nx xg)   = let x' = nx xg in foldl f z x'
  foldl f z (Some x nx xg) = let x' = nx xg in foldl f (f z x) x'

  foldl' _ z Done           = z
  foldl' f z (Skip nx xg)   = let !fld = foldl' f z (nx xg) in fld
  foldl' f z (Some x nx xg) = let !fzx = f z x; !fld = foldl' f fzx (nx xg) in fld
