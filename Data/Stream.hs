{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Stream (
  -- ** Stream
  Stream(..)

  -- ** Constructors
  -- *** Infinite
  , repeat

  -- ** Accessors
  , head, headElse, last, lastElse, uncons

  -- ** Slices
  , tail, init, take, drop

  , intercalate
  ) where

import Prelude hiding (repeat, head, last, uncons, tail, init, take, drop)
import GHC.Exts (IsList(..))
import Data.Foldable (Foldable(foldr', foldl'))
import Data.Maybe (maybe, fromMaybe)

data Step g a = Some a !g | Skip !g | Done

{-# INLINE [0] emptyStep #-}
emptyStep _ = Done

data Stream a = forall g. Stream !(g -> Step g a) !g

instance IsList (Stream a) where
  type Item (Stream a) = a

  {-# INLINE [1] fromList #-}
  fromList ls = Stream nx ls where
    {-# INLINE [0] nx #-}
    nx []     = Done
    nx (x:xs) = Some x xs

  {-# INLINE [1] toList #-}
  toList (Stream nx sg) = toList' sg where
    {-# INLINE [0] toList' #-}
    toList' !g = case nx g of
      Done      -> []
      Skip g'   -> toList' g'
      Some x g' -> x : toList' g'

instance Eq a => Eq (Stream a) where
  a == b = (toList a) == (toList b)

instance Semigroup (Stream a) where
  {-# INLINE [1] (<>) #-}
  (Stream nxa sga) <> (Stream nxb sgb) = Stream nx' (Left sga) where
    {-# INLINE [0] nx' #-}
    nx' (Left !g)  = case nxa g of
      Done      -> nx' (Right sgb)
      Skip g'   -> Skip (Left g')
      Some x g' -> (Some x (Left g'))
    nx' (Right !g) = case nxb g of
      Done      -> Done
      Skip g'   -> Skip (Right g')
      Some x g' -> (Some x (Right g'))

instance Monoid (Stream a) where
  {-# INLINE [0] mempty #-}
  mempty = Stream emptyStep ()

instance Functor Stream where
  {-# INLINE [1] fmap #-}
  fmap f (Stream nx sg) = Stream nx' sg where
    {-# INLINE [0] nx' #-}
    nx' !g = case nx g of
      Done      -> Done
      Skip g'   -> Skip g'
      Some x g' -> (Some (f x) g')

instance Applicative Stream where
  {-# INLINE [0] pure #-}
  pure x = Stream nx (Just x) where
    {-# INLINE [0] nx #-}
    nx Nothing  = Done
    nx (Just x) = Some x Nothing

  {-# INLINE [1] liftA2 #-}
  liftA2 f (Stream nxa sga) (Stream nxb sgb) = Stream nx' (sga, sgb) where
    {-# INLINE [0] nx' #-}
    nx' (!a, !b) = case nxa a of
      Done       -> Done
      Skip ag    -> Skip (ag, b)
      Some a' ag -> case nxb b of
        Done       -> nx' (ag, sgb)
        Skip bg    -> Skip (a, bg)
        Some b' bg -> Some (f a' b') (a, bg)

instance Foldable Stream where
  {-# INLINE [1] foldr #-}
  foldr f z (Stream nx sg) = flr' z sg where
    {-# INLINE [0] flr' #-}
    flr' z' !g = case nx g of
      Done      -> z'
      Skip g'   -> flr' z' g'
      Some x g' -> f x $ flr' z' g'

  {-# INLINE [1] foldr' #-}
  foldr' f z (Stream nx sg) = flr' z sg where
    {-# INLINE [0] flr' #-}
    flr' z' !g = case nx g of
      Done      -> z'
      Skip g'   -> let !fz = flr' z' g' in fz
      Some x g' -> let !fz = flr' z' g'; !fxz = f x fz in fxz

  {-# INLINE [1] foldl #-}
  foldl f z (Stream nx sg) = fll' z sg where
    {-# INLINE [0] fll' #-}
    fll' z' !g = case nx g of
      Done      -> z'
      Skip g'   -> fll' z' g'
      Some x g' -> fll' (f z' x) g'

  {-# INLINE [1] foldl' #-}
  foldl' f z (Stream nx sg) = fll' z sg where
    {-# INLINE [0] fll' #-}
    fll' z' !g = case nx g of
      Done      -> z'
      Skip g'   -> let !fz = fll' z' g' in fz
      Some x g' -> let !fxz = f z' x; !fz = fll' fz g' in fz





--------------------------------------------------
-- Constructors
--------------------------------------------------

-- \(\mathcal{O}(1)\). Creates a stream consisting of a single element repeated infinitely.
repeat :: a -> Stream a
repeat x = Stream nx () where
  nx _ = Some x ()






--------------------------------------------------
-- Accessors
--------------------------------------------------

-- | \(\mathcal{O}(1)\) (Skips notwithstanding). Retrieve the first element available in a stream, or Nothing if there isn't one.
head :: Stream a -> Maybe a
{-# INLINE [1] head #-}
head (Stream nx sg) = head' nx sg

{-# INLINE [0] head' #-}
head' :: (g -> Step g a) -> g -> Maybe a
head' nx !g = case nx g of
  Done      -> Nothing
  Skip g'   -> head' nx g'
  Some x g' -> Just x

-- | \(\mathcal{O}(1)\). Retrieve the first element available in a stream, or a provided default if there isn't one.
headElse :: a -> Stream a -> a
{-# INLINE [0] headElse #-}
headElse e = fromMaybe e . head

-- | \(\mathcal{O}(n)\). Retrieve the last element available in a stream, or Nothing if there isn't one. Will hang on non-terminating streams.
last :: Stream a -> Maybe a
{-# INLINE [1] last #-}
last (Stream nx sg) = last' Nothing sg where
  {-# INLINE [0] last' #-}
  last' l !g = case nx g of
    Done      -> l
    Skip g'   -> last' l g'
    Some x g' -> last' (Just x) g'

-- | \(\mathcal{O}(n)\). Retrieve the last element available in a stream, or a provided default if there isn't one. Will hang on non-terminating streams.
lastElse :: a -> Stream a -> a
{-# INLINE [0] lastElse #-}
lastElse e = fromMaybe e . last

-- | \(\mathcal{O}(1)\).
uncons :: Stream a -> Maybe (a, Stream a)
{-# INLINE [1] uncons #-}
uncons (Stream nx sg) = uncons' nx sg

uncons' :: (g -> Step g a) -> g -> Maybe (a, Stream a)
{-# INLINE [0] uncons' #-}
uncons' nx !g = case nx g of
  Done      -> Nothing
  Skip g'   -> uncons' nx g'
  Some x g' -> Just (x, Stream nx g')





--------------------------------------------------
-- Slices
--------------------------------------------------

-- | \(\mathcal{O}(1)\) (Skips notwithstanding). Creates a new stream containing all elements of an existing stream except for first available.
tail :: Stream a -> Stream a
{-# INLINE [1] tail #-}
tail (Stream nx sg) = tail' sg where
  {-# INLINE [0] tail' #-}
  tail' !g = case nx g of
    Done      -> mempty
    Skip g'   -> tail' g'
    Some _ g' -> Stream nx g'

-- | \(\mathcal{O}(1)\). Creates a new stream containing all elements of an existing stream except the last available before it terminates. If it doesn't terminate then it's identical to the input.
init :: Stream a -> Stream a
{-# INLINE [1] init #-}
init (Stream nx sg) = Stream nx' sg where
  {-# INLINE [0] nx' #-}
  nx' !g = case nx g of
    Done      -> Done
    Skip g'   -> Skip g'
    Some x g' -> isEnd x g' g'
  {-# INLINE [0] isEnd #-}
  isEnd x og g = case nx g of
    Done      -> Done
    Skip g'   -> isEnd x og g'
    Some _ _  -> Some x og

-- | \(\mathcal{O}()\).
take :: Int -> Stream a -> Stream a
{-# INLINE [1] take #-}
take n (Stream nx sg) = Stream nx' (n, sg) where
  {-# INLINE [0] nx' #-}
  nx' (n, !g) = case nx g of
    Done      -> Done
    Skip g'   -> Skip (n, g')
    Some x g' -> if n <= 0 then Done else Some x (n - 1, g')

drop :: Int -> Stream a -> Stream a
{-# INLINE [1] drop #-}
drop n (Stream nx sg) = Stream nx' (n, sg) where
  {-# INLINE [0] nx' #-}
  nx' (n, !g) = case nx g of
    Done      -> Done
    Skip g'   -> Skip (n, g')
    Some x g' -> if n <= 0 then Skip (n - 1, g') else Some x (0, g')




intercalate :: Stream a -> Stream (Stream a) -> Stream a
{-# INLINE [1] intercalate #-}
intercalate e (Stream nx sg) = Stream nx' (Nothing, sg) where
  {-# INLINE [0] nx' #-}
  nx' (Nothing, !g) = case nx g of
    Done      -> Done
    Skip g'   -> Skip (Nothing, g')
    Some x g' -> nx' (Just (Left x), g')
  nx' (Just (Left (Stream inx isg)), !g) = case inx isg of
    Done      -> maybe Done (\_ -> nx' (Just (Right e), g)) (head' nx g)
    Skip g'   -> Skip (Just (Left (Stream inx g')), g)
    Some x g' -> Some x (Just (Left (Stream inx g')), g)
  nx' (Just (Right (Stream enx esg)), !g) = case enx esg of
    Done      -> nx' (Nothing, g)
    Skip g'   -> Skip (Just (Right (Stream enx g')), g)
    Some x g' -> Some x (Just (Right (Stream enx g')), g)