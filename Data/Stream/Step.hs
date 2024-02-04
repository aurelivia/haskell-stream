module Data.Stream.Step (
  Step(..)
  , emptyStep
  ) where

data Step a g = Some a !g | Skip !g | Done

instance Functor (Step a) where
  {-# INLINE [0] fmap #-}
  fmap _  Done      = Done
  fmap f (Skip g)   = Skip (f g)
  fmap f (Some x g) = Some x (f g)

{-# INLINE [0] emptyStep #-}
emptyStep _ = Done
