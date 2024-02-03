{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Stream
  ( Stream(..)
  , Step(..)
  ) where

import GHC.Exts (IsList(..))

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
      Skip g'   -> nx' (Left g')
      Some x g' -> (Some x (Left g'))
    nx' (Right !g) = case nxb g of
      Done      -> Done
      Skip g'   -> nx' (Right g')
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
      Skip g'   -> nx' g'
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
      Skip ag    -> nx' (ag, b)
      Some a' ag -> case nxb b of
        Done       -> nx' (ag, sgb)
        Skip bg    -> nx' (a, bg)
        Some b' bg -> Some (f a' b') (a, bg)