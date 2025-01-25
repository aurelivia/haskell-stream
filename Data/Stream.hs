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

data Stream a = Done | forall g. Skip !(g -> Stream a) !g | forall g. Some a !(g -> Stream a) !g

--------------------------------------------------
-- Constructors
--------------------------------------------------

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
