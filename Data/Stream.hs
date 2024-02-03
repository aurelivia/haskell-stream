{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Stream
  ( Stream(..)
  , Step(..)
  ) where

import GHC.Exts (IsList(..))

data Step a g = Some a !g | Skip !g | Done

data Stream a = forall g. Stream !(g -> Step a g) !g

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
