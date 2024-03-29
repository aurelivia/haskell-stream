{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Stream (
  -- ** Stream
  Stream(.., (:<>)), Step(..)

  -- ** Constructors
  , cons, snoc
  -- *** Infinite
  , repeat, cycle, iterate

  -- ** Accessors
  , head, headElse, last, lastElse, uncons, (!?), length, isDone

  -- ** Slices
  , tail, init, take, drop, slice, splitAt, takeWhile, dropWhile, span, spanMaybe, groupEvery

  -- ** Folds
  , intercalate, transpose
  , concat, concatMap, concat2, concat3, concat4, concat5, concat6, concat7
  ) where

import Prelude hiding
  ( repeat, head, last, uncons, tail, init, take, drop, cycle, iterate, takeWhile, dropWhile
  , transpose, concatMap, length, concat, span, splitAt
  )
import GHC.Exts (IsList(..), IsString(..))
import Control.DeepSeq (NFData(..), NFData1(..), rnf1)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Fix (MonadFix(..))
-- import Control.Monad.Zip (MonadZip(..))
import Data.Foldable (Foldable(foldr', foldl'))
import Data.Bifunctor (first)
import Data.Sequence (Seq(..), ViewR(..), (<|), (|>))
import qualified Data.Sequence as Seq

import Data.Stream.Step

data Stream a = forall g. Stream !(g -> Step a g) !g

infixr 5 :<>
pattern (:<>) :: a -> Stream a -> Stream a
pattern x :<> xs <- (uncons -> Just (x, xs)) where
  x :<> xs = cons x xs

instance Show a => Show (Stream a) where
  {-# INLINE [0] showsPrec #-}
  showsPrec p = showsPrec p . toList

instance Read a => Read (Stream a) where
  {-# INLINE [0] readsPrec #-}
  readsPrec p = map (first fromList) . readsPrec p

instance IsList (Stream a) where
  type Item (Stream a) = a

  {-# INLINE [1] fromList #-}
  fromList = Stream nx where
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

instance IsString (Stream Char) where
  {-# INLINE [0] fromString #-}
  fromString = fromList

instance Eq a => Eq (Stream a) where
  {-# INLINE [0] (==) #-}
  a == b = case (uncons a, uncons b) of
    (Nothing, Nothing) -> True
    (Nothing, Just _)  -> False
    (Just _, Nothing)  -> False
    (Just (ha, as), Just (hb, bs)) -> (ha == hb) && let !q = (as == bs) in q

instance Ord a => Ord (Stream a) where
  {-# INLINE [0] compare #-}
  compare a b = case (uncons a, uncons b) of
    (Nothing, Nothing) -> EQ
    (Nothing, Just _)  -> LT
    (Just _, Nothing)  -> GT
    (Just (ha, as), Just (hb, bs)) -> let !c = compare ha hb in if c == EQ then (let !q = compare as bs in q) else c

instance Semigroup (Stream a) where
  {-# INLINE [1] (<>) #-}
  (Stream nxa sga) <> (Stream nxb sgb) = Stream nx' (Left sga) where
    {-# INLINE [0] nx' #-}
    nx' (Left !g)  = case nxa g of
      Done      -> nx' (Right sgb)
      Skip g'   -> Skip (Left g')
      Some x g' -> Some x (Left g')
    nx' (Right !g) = case nxb g of
      Done      -> Done
      Skip g'   -> Skip (Right g')
      Some x g' -> Some x (Right g')

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
      Some x g' -> Some (f x) g'

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

instance Alternative Stream where
  {-# INLINE [0] empty #-}
  empty = mempty
  {-# INLINE [1] (<|>) #-}
  (<|>) = (<>)

instance Monad Stream where
  {-# INLINE [1] (>>=) #-}
  (Stream nx sg) >>= f = Stream nx' (Nothing, sg) where
    {-# INLINE [0] nx' #-}
    nx' (Nothing, !g) = case nx g of
      Done      -> Done
      Skip g'   -> Skip (Nothing, g')
      Some x g' -> nx' (Just $ f x, g')
    nx' (Just (Stream ifx !ig), !g) = case ifx ig of
      Done        -> nx' (Nothing, g)
      Skip ig'    -> Skip (Just (Stream ifx ig'), g)
      Some ix ig' -> Some ix (Just (Stream ifx ig'), g)

instance MonadPlus Stream

instance MonadFail Stream where
  {-# INLINE [0] fail #-}
  fail _ = mempty
--
-- instance MonadFix Stream where
--   {-# INLINE [1] mfix #-}
--   mfix f (Stream nx sg) = Stream nx' (0, sg) where
--     {-# INLINE [0] nx' #-}
--     nx' (n, !g) = case nx g of
--       Done       -> Done
--       Skip g'    -> Skip (n, g')
--       Some fx _ -> case head $ drop n (f fx) of
--         Nothing -> Done
--         Just x  -> Some x (n + 1, g)

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
      Some x g' -> let !fxz = f z' x; !fz = fll' fxz g' in fz

instance Traversable Stream where
  {-# INLINE [1] traverse #-}
  traverse f s = fromList <$> foldr appCons (pure []) s where
    appCons x = liftA2 (:) (f x)

instance NFData a => NFData (Stream a) where
  {-# INLINE [0] rnf #-}
  rnf = rnf1

instance NFData1 Stream where
  {-# INLINE [0] liftRnf #-}
  liftRnf r (Stream nx sg) = liftRnf' sg where
    {-# INLINE [0] liftRnf' #-}
    liftRnf' !g = case nx g of
      Done      -> ()
      Skip g'   -> liftRnf' g'
      Some x g' -> r x `seq` liftRnf' g'





--------------------------------------------------
-- Constructors
--------------------------------------------------

{-# INLINE [0] cons #-}
cons :: a -> Stream a -> Stream a
cons x = (<>) (pure x)

{-# INLINE [0] snoc #-}
snoc :: Stream a -> a -> Stream a
snoc xs x = xs <> pure x

-- | Creates a stream consisting of a single element repeated infinitely.
{-# INLINE [0] repeat #-}
repeat :: a -> Stream a
repeat x = Stream nx () where
  {-# INLINE [0] nx #-}
  nx _ = Some x ()

-- | Turns a finite list into an infinite one by repeating it.
{-# INLINE [1] cycle #-}
cycle :: Stream a -> Stream a
cycle (Stream nx sg) = Stream nx' sg where
  {-# INLINE [0] nx' #-}
  nx' !g = case nx g of
    Done      -> nx' sg
    Skip g'   -> Skip g'
    Some x g' -> Some x g'

{-# INLINE [1] iterate #-}
iterate :: (a -> a) -> a -> Stream a
iterate f x = Stream nx x where
  {-# INLINE [0] nx #-}
  nx !x = Some x (f x)





--------------------------------------------------
-- Accessors
--------------------------------------------------

-- | \(\mathcal{O}(1)\) (Skips notwithstanding). Retrieve the first element available in a stream, or Nothing if there isn't one.
{-# INLINE [1] head #-}
head :: Stream a -> Maybe a
head (Stream nx sg) = head' nx sg

{-# INLINE [0] head' #-}
head' :: (g -> Step a g) -> g -> Maybe a
head' nx !g = case nx g of
  Done      -> Nothing
  Skip g'   -> head' nx g'
  Some x g' -> Just x

-- | \(\mathcal{O}(1)\). Retrieve the first element available in a stream, or a provided default if there isn't one.
{-# INLINE [0] headElse #-}
headElse :: a -> Stream a -> a
headElse e = fromMaybe e . head

-- | \(\mathcal{O}(n)\). Retrieve the last element available in a stream, or Nothing if there isn't one. Will hang on non-terminating streams.
{-# INLINE [1] last #-}
last :: Stream a -> Maybe a
last (Stream nx sg) = last' Nothing nx sg

{-# INLINE [0] last' #-}
last' l nx !g = case nx g of
  Done      -> l
  Skip g'   -> last' l nx g'
  Some x g' -> last' (Just x) nx g'

-- | \(\mathcal{O}(n)\). Retrieve the last element available in a stream, or a provided default if there isn't one. Will hang on non-terminating streams.
{-# INLINE [0] lastElse #-}
lastElse :: a -> Stream a -> a
lastElse e = fromMaybe e . last

-- | \(\mathcal{O}(1)\). Produce a tuple consisting of @(head x, tail x)@ if a stream has any elements, otherwise Nothing.
{-# INLINE [1] uncons #-}
uncons :: Stream a -> Maybe (a, Stream a)
uncons (Stream nx sg) = uncons' nx sg

{-# INLINE [0] uncons' #-}
uncons' :: (g -> Step a g) -> g -> Maybe (a, Stream a)
uncons' nx !g = case nx g of
  Done      -> Nothing
  Skip g'   -> uncons' nx g'
  Some x g' -> Just (x, Stream nx g')

-- | \(\mathcal{O}(n)\). Retrieve the \mathcal{n}th available element, or Nothing if none exists.
(!?) :: Stream a -> Int -> Maybe a
infixl 9 !?
{-# INLINE [0] (!?) #-}
(Stream nx sg) !? i = if i < 0 then Nothing else idx' i nx sg

{-# INLINE [0] idx' #-}
idx' :: Int -> (g -> Step a g) -> g -> Maybe a
idx' i nx !g = case nx g of
  Done      -> Nothing
  Skip g'   -> idx' i nx g'
  Some x g' -> if i == 0 then Just x else idx' (i - 1) nx g'

{-# INLINE [0] length #-}
length :: Stream a -> Int
length = foldl' (\l x -> x `seq` l + 1) 0

{-# INLINE [0] isDone #-}
isDone :: Stream a -> Bool
isDone (Stream nx sg) = case nx sg of
  Done      -> True
  Skip g'   -> isDone (Stream nx g')
  Some _ g' -> False





--------------------------------------------------
-- Slices
--------------------------------------------------

-- | \(\mathcal{O}(1)\) (Skips notwithstanding). Creates a new stream containing all elements of an existing stream except for first available.
{-# INLINE [1] tail #-}
tail :: Stream a -> Stream a
tail (Stream nx sg) = tail' nx sg

{-# INLINE [0] tail' #-}
tail' :: (g -> Step a g) -> g -> Stream a
tail' nx !g = case nx g of
  Done      -> mempty
  Skip g'   -> tail' nx g'
  Some _ g' -> Stream nx g'

-- | \(\mathcal{O}(1)\). Creates a new stream containing all elements of an existing stream except the last available before it terminates. If it doesn't terminate then it's identical to the input.
{-# INLINE [1] init #-}
init :: Stream a -> Stream a
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

-- | \(\mathcal{O}()\). Produce a new stream consisting of only the first \mathcal{n} elements of a given stream.
{-# INLINE [1] take #-}
take :: Int -> Stream a -> Stream a
take n (Stream nx sg) = Stream nx' (n, sg) where
  {-# INLINE [0] nx' #-}
  nx' (n, !g) = case nx g of
    Done      -> Done
    Skip g'   -> Skip (n, g')
    Some x g' -> if n <= 0 then Done else Some x (n - 1, g')

-- | \(\mathcal{O}(n)\). Produce a new stream consisting of all elements from a given stream except for the first \mathcal{n}.
{-# INLINE [1] drop #-}
drop :: Int -> Stream a -> Stream a
drop n (Stream nx sg) = Stream nx' (n, sg) where
  {-# INLINE [0] nx' #-}
  nx' (n, !g) = case nx g of
    Done      -> Done
    Skip g'   -> Skip (n, g')
    Some x g' -> if n <= 0 then Some x (0, g') else Skip (n - 1, g')

-- | \(\mathcal{O}(n)\). Produce a new stream consisting of only the next \mathcal{n} available elements after dropping \mathcal{i}. Equivalent to @take n . drop i@.
{-# INLINE [1] slice #-}
slice :: Int -> Int -> Stream a -> Stream a
slice i n = take n . drop i

{-# INLINE [1] splitAt #-}
splitAt :: Int -> Stream a -> (Stream a, Stream a)
splitAt i s = (take i s, drop i s)

{-# INLINE [1] takeWhile #-}
takeWhile :: (a -> Bool) -> Stream a -> Stream a
takeWhile f (Stream nx sg) = Stream nx' sg where
  {-# INLINE [0] nx' #-}
  nx' !g = case nx g of
    Done      -> Done
    Skip g'   -> Skip g'
    Some x g' -> if f x then Some x g' else Done

{-# INLINE [1] dropWhile #-}
dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile f (Stream nx sg) = Stream nx' (Left sg) where
  {-# INLINE [0] nx' #-}
  nx' (Right !g) = Right <$> nx g
  nx' (Left  !g) = case nx g of
    Done      -> Done
    Skip g'   -> Skip (Left g')
    Some x g' -> if f x then Skip (Left g') else Some x (Right g')

span :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
span f s = (takeWhile f s, dropWhile f s)

{-# INLINE [1] spanMaybe #-}
spanMaybe :: Stream (Maybe a) -> (Stream a, Stream (Maybe a))
spanMaybe s@(Stream nx sg) = (Stream take' sg, Stream drop' (Left sg)) where
  {-# INLINE [0] take' #-}
  take' !g = case nx g of
    Done      -> Done
    Skip g'   -> Skip g'
    Some x g' -> maybe Done (`Some` g') x
  {-# INLINE [0] drop' #-}
  drop' (Right !g)  = Right <$> nx g
  drop' (Left  !g) = case nx g of
    Done      -> Done
    Skip g'   -> Skip (Left g')
    Some (Just _) g' -> Skip (Left g')
    Some Nothing  g' -> case nx g' of
      Done      -> Done
      Skip g'   -> Skip (Right g')
      Some x g' -> Some x (Right g')

{-# INLINE [1] groupEvery #-}
groupEvery :: Int -> Stream a -> Stream (Stream a)
groupEvery i s = Stream nx s where
  {-# INLINE [0] nx #-}
  nx s = let (t, d) = splitAt i s in if isDone t then Done else Some t d





{-# INLINE [1] intercalate #-}
intercalate :: Stream a -> Stream (Stream a) -> Stream a
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

{-# INLINE [1] transpose #-}
transpose :: Stream (Stream a) -> Stream (Maybe a)
transpose (Stream nx sg) = Stream nx' (init sg) where
  {-# INLINE [0] init #-}
  init !g = case nx g of
    Done      -> (Seq.empty, Seq.empty, g)
    Skip g'   -> init g'
    Some x g' -> (Seq.empty, Seq.singleton x, g')
  {-# INLINE [0] nx' #-}
  nx' (Seq.Empty, Seq.Empty, _) = Done
  nx' (prev, Seq.Empty, !g) = case nx g of
    Done      -> Some Nothing (Seq.empty, prev, g)
    Skip g'   -> Skip (prev, Seq.empty, g')
    Some x g' -> nx' (prev, Seq.singleton x, g')
  nx' (prev, (Stream ix ig) :<| nxt, !g) = case ix ig of
    Done      -> nx' (prev, nxt, g)
    Skip g'   -> Skip (prev, Stream ix g' <| nxt, g)
    Some x g' -> Some (Just x) (prev |> Stream ix g', nxt, g)

concat :: Stream (Stream a) -> Stream a
concat (Stream nx sg) = Stream (concat' nx id) (Nothing, sg)

{-# INLINE [0] concat' #-}
concat' :: (g -> Step a g) -> (a -> Stream b) -> (Maybe (Stream b), g) -> Step b (Maybe (Stream b), g)
concat' nx f (Nothing, !g) = case nx g of
  Done      -> Done
  Skip g'   -> Skip (Nothing, g')
  Some x g' -> concat' nx f (Just (f x), g')
concat' nx f (i@(Just (Stream ix !ig)), !g) = case ix ig of
  Done      -> concat' nx f (Nothing, g)
  Skip g'   -> Skip (i, g)
  Some x g' -> Some x (Just (Stream ix g'), g)

concatMap :: (a -> Stream b) -> Stream a -> Stream b
concatMap f (Stream nx sg) = Stream (concat' nx f) (Nothing, sg)

concatAppend :: Stream a -> Stream (Stream a) -> Stream a
concatAppend zs (Stream nx sg) = Stream (concat' nx id) (Just zs, sg)

concat2Append :: Stream a -> Stream (Stream (Stream a)) -> Stream a
concat2Append zs (Stream nx sg) = Stream (concat' nx concat) (Just zs, sg)

concat2 :: Stream (Stream (Stream a)) -> Stream a
concat2 (Stream nx sg) = Stream (concat' nx concat) (Nothing, sg)

concat3 :: Stream (Stream (Stream (Stream a))) -> Stream a
concat3 (Stream nx sg) = Stream (concat' nx concat2) (Nothing, sg)

concat4 :: Stream (Stream (Stream (Stream (Stream a)))) -> Stream a
concat4 (Stream nx sg) = Stream (concat' nx concat3) (Nothing, sg)

concat5 :: Stream (Stream (Stream (Stream (Stream (Stream a))))) -> Stream a
concat5 (Stream nx sg) = Stream (concat' nx concat4) (Nothing, sg)

concat6 :: Stream (Stream (Stream (Stream (Stream (Stream (Stream a)))))) -> Stream a
concat6 (Stream nx sg) = Stream (concat' nx concat5) (Nothing, sg)

concat7 :: Stream (Stream (Stream (Stream (Stream (Stream (Stream (Stream a))))))) -> Stream a
concat7 (Stream nx sg) = Stream (concat' nx concat6) (Nothing, sg)