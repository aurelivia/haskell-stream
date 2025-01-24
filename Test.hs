{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import Prelude hiding (head, tail, intercalate)
import qualified Prelude as P
import qualified Data.List as L
import Data.Stream (Stream)
import qualified Data.Stream as S
import GHC.Exts (toList, fromList)

main = defaultMain $ testGroup "Stream"
  [ toFromList
  ]

{-# INLINE oneFive #-}
oneFive :: [Int]
oneFive = [ 1, 2, 3, 4, 5 ]

{-# INLINE oneFiveS #-}
oneFiveS :: Stream Int
oneFiveS = fromList oneFive

{-# INLINE toStream #-}
toStream :: [a] -> Stream a
toStream = fromList

toFromList = testCase "To/From Lists" $ (toList . toStream) oneFive @?= oneFive