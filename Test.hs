{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Prelude as P
import qualified Data.List as L
import Data.Stream
import qualified Data.Stream as S
import GHC.Exts (toList, fromList)

main = defaultMain $ testGroup "Stream"
  [ toFromList
  , semigroup
  , functor
  , applicative
  , Main.intercalate
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

timesTwo :: Stream (Int -> Int)
timesTwo = pure $ (*) 2

timesFour :: Stream (Int -> Int)
timesFour = pure $ (*) 4

applicative = testGroup "Applicative"
  [ testCase "pure" $ (toList $ pure @Stream 1) @?= [ 1 ]
  , testCase "Identity" $ (toList $ pure id <*> oneFiveS) @?= oneFive
  , testCase "Composition" $ (toList $ pure (.) <*> timesTwo <*> timesFour <*> oneFiveS) @?= (toList $ timesTwo <*> (timesFour <*> oneFiveS))
  , testCase "Homomorphism" $ (toList $ timesTwo <*> pure @Stream 2) @?= (toList $ pure @Stream 4)
  , testCase "Interchange" $ (toList $ timesTwo <*> pure @Stream 2) @?= (toList $ pure ($ 2) <*> timesTwo)
  , testCase "Functor" $ (toList $ fmap ((*) 2) oneFiveS) @?= (toList $ pure ((*) 2) <*> oneFiveS)
  ]

space :: [Int]
space = P.take 3 $ P.repeat 9
loopy :: [[Int]]
loopy = P.take 3 $ P.repeat oneFive
spaceS :: Stream Int
spaceS = S.take 3 $ S.repeat 9
loopyS :: Stream (Stream Int)
loopyS = S.take 3 $ S.repeat oneFiveS

intercalate = testCase "intercalate" $ (toList $ S.intercalate spaceS loopyS) @?= (L.intercalate space loopy)