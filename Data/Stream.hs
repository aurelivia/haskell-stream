{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Stream (
  -- ** Stream
  Stream(..)
  ) where

import Prelude
import GHC.Exts (IsList(..), IsString(..))

data Stream a = Done | forall g. Skip !(g -> Stream a) !g | forall g. Some a !(g -> Stream a) !g

instance IsList (Stream a) where
  type Item (Stream a) = a

  fromList [] = Done
  fromList (x:xs) = Some x fromList xs

  toList Done           = []
  toList (Skip nx sg)   = toList (nx sg)
  toList (Some x nx sg) = x : toList (nx sg)