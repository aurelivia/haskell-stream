{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Stream
import GHC.Exts (toList, fromList)

main = defaultMain $ testGroup "Stream"
  [ toFromList
  , semigroup
  , functor
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

functor = testGroup "Functor"
  [ testCase "Identity" $ (toList $ fmap id $ toStream oneFive) @?= oneFive
  , testCase "Composition" $ (toList $ fmap ((+) 1 . (*) 2) $ toStream oneFive) @?= (toList $ fmap ((+) 1) . fmap ((*) 2) $ toStream oneFive)
  ]