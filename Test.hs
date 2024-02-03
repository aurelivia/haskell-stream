{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Stream
import GHC.Exts (toList, fromList)

main = defaultMain $ testGroup "Stream"
  [ toFromList
  , semigroup
  ]

oneFive :: [Int]
oneFive = [ 1, 2, 3, 4, 5 ]

toStream :: [a] -> Stream a
toStream = fromList

toFromList = testCase "To/From Lists" $ (toList . toStream) oneFive @?= oneFive

semigroup = testGroup "Semigroup"
  [ testCase "Two Empty" $ (toList $ (toStream []) <> (toStream @[Int] [])) @?= []
  , testCase "Left Full" $ (toList $ (toStream oneFive) <> (toStream [])) @?= oneFive
  , testCase "Rght Full" $ (toList $ (toStream []) <> (toStream oneFive)) @?= oneFive
  , testCase "Both Full" $ (toList $ (toStream oneFive) <> (toStream oneFive)) @?= oneFive ++ oneFive
  ]
