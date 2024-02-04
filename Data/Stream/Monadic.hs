{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Data.Stream.Monadic (
  -- ** Stream
  Stream(..), Step(..)

  -- ** Constructors
  , cons, snoc
  -- *** Infinite
  , repeat, cycle, iterate

  -- ** Accessors
  , head, headElse, last, lastElse, uncons, (!?)

  -- ** Slices
  , tail, init, take, drop, slice, takeWhile, dropWhile

  , intercalate
  ) where

import Prelude hiding
  (repeat, head, last, uncons, tail, init, take, drop, cycle, iterate, takeWhile, dropWhile
  )
import GHC.Exts (IsList(..), IsString(..))
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Fix (MonadFix(..))
-- import Control.Monad.Zip (MonadZip(..))
import Data.Foldable (Foldable(foldr', foldl'))
import Data.Maybe (maybe, fromMaybe)

import Data.Stream.Step

data Stream (m a) = forall g. Stream !(g -> m (Step a g)) !g


instance Monad m => IsList (Stream (m a)) where
  type Item (Stream (m a)) = a

  {-# INLINE [1] fromList #-}
  fromList ls = Stream nx ls where
    {-# INLINE [0] nx #-}
    nx []     = return Done
    nx (x:xs) = return $ Some x xs

  {-# INLINE [1] toList #-}
  toList (Stream nx sg) = toList' sg where
    {-# INLINE [0] toList' #-}
    toList' !g = (nx g) >>= \case
      Done      -> []
      Skip g'   -> toList' g'
      Some x g' -> x : toList' g'