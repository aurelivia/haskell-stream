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

{-# INLINE oneFive #-}
oneFive :: [Int]
oneFive = [ 1, 2, 3, 4, 5 ]

{-# INLINE oneFiveS #-}
oneFiveS :: Stream Int
oneFiveS = fromList oneFive

{-# INLINE sixTen #-}
sixTen :: [Int]
sixTen = [ 6, 7, 8, 9, 10 ]

{-# INLINE sixTenS #-}
sixTenS :: Stream Int
sixTenS = fromList sixTen

{-# INLINE elvFif #-}
elvFif :: [Int]
elvFif = [ 11, 12, 13, 14, 15 ]

{-# INLINE elvFifS #-}
elvFifS :: Stream Int
elvFifS = fromList elvFif

{-# INLINE toStream #-}
toStream :: [a] -> Stream a
toStream = fromList

toFromList = testCase "To/From Lists" $ (toList . toStream) oneFive @?= oneFive

semigroup = testGroup "Semigroup"
  [ testCase "Two Empty" $ (toList $ (toStream []) <> (toStream @[Int] [])) @?= []
  , testCase "Left Full" $ (toList $ oneFiveS <> (toStream [])) @?= oneFive
  , testCase "Rght Full" $ (toList $ (toStream []) <> oneFiveS) @?= oneFive
  , testCase "Both Full" $ (toList $ oneFiveS <> oneFiveS) @?= oneFive ++ oneFive
  , testCase "Associativity" $ (toList $ oneFiveS <> (sixTenS <> elvFifS)) @?= (toList $ (oneFiveS <> sixTenS) <> elvFifS)
  ]

monoid = testGroup "Monoid"
  [ testCase "Right Identity" $ (toList $ oneFiveS <> (mempty :: Stream Int)) @?= oneFive
  , testCase "Left Identity" $ (toList $ (mempty :: Stream Int) <> oneFiveS) @?= oneFive
  ]

functor = testGroup "Functor"
  [ testCase "Identity" $ (toList $ fmap id oneFiveS) @?= oneFive
  , testCase "Composition" $ (toList $ fmap ((+) 1 . (*) 2) oneFiveS) @?= (toList $ fmap ((+) 1) . fmap ((*) 2) $ oneFiveS)
  ]