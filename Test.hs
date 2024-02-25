{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import Prelude hiding (head, tail, intercalate)
import qualified Prelude as P
import qualified Data.List as L
import Data.Stream
import qualified Data.Stream as S
import GHC.Exts (toList, fromList)
import Data.Foldable (foldr, foldr', foldl, foldl')

main = defaultMain $ testGroup "Stream"
  [ toFromList
  , semigroup
  , functor
  , applicative
  , foldable
  , accessors
  , slices
  , folds
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
  , testCase "Associativity" $ (oneFiveS <> (sixTenS <> elvFifS)) @?= ((oneFiveS <> sixTenS) <> elvFifS)
  ]

monoid = testGroup "Monoid"
  [ testCase "Right Identity" $ (toList $ oneFiveS <> (mempty :: Stream Int)) @?= oneFive
  , testCase "Left Identity" $ (toList $ (mempty :: Stream Int) <> oneFiveS) @?= oneFive
  ]

functor = testGroup "Functor"
  [ testCase "Identity" $ (toList $ fmap id oneFiveS) @?= oneFive
  , testCase "Composition" $ (fmap ((+) 1 . (*) 2) oneFiveS) @?= (fmap ((+) 1) . fmap ((*) 2) $ oneFiveS)
  ]

timesTwo :: Stream (Int -> Int)
timesTwo = pure $ (*) 2

timesFour :: Stream (Int -> Int)
timesFour = pure $ (*) 4

applicative = testGroup "Applicative"
  [ testCase "pure" $ toList (pure @Stream 1) @?= [ 1 ]
  , testCase "Identity" $ (toList $ pure id <*> oneFiveS) @?= oneFive
  , testCase "Composition" $ (pure (.) <*> timesTwo <*> timesFour <*> oneFiveS) @?= (timesTwo <*> (timesFour <*> oneFiveS))
  , testCase "Homomorphism" $ (timesTwo <*> pure @Stream 2) @?= (pure @Stream 4)
  , testCase "Interchange" $ (timesTwo <*> pure @Stream 2) @?= (pure ($ 2) <*> timesTwo)
  , testCase "Functor" $ (fmap ((*) 2) oneFiveS) @?= (pure ((*) 2) <*> oneFiveS)
  ]

foldable = testGroup "Foldable"
  [ testCase "foldr" $ foldr (\x xs -> show x ++ xs) "" oneFiveS @?= "12345"
  , testCase "foldr'" $ foldr' (\x xs -> show x ++ xs) "" oneFiveS @?= "12345"
  , testCase "foldl" $ foldl (\xs x -> show x ++ xs) "" oneFiveS @?= "54321"
  , testCase "foldl'" $ foldl' (\xs x -> show x ++ xs) "" oneFiveS @?= "54321"
  ]

accessors = testGroup "Accessors"
  [ testCase "head" $ S.head oneFiveS @?= Just 1
  , testCase "length" $ S.length oneFiveS @?= 5
  , testGroup "isDone" [ testCase "True" $ S.isDone mempty @?= True, testCase "False" $ S.isDone oneFiveS @?= False ]
  ]

slices = testGroup "Slices"
  [ testCase "take" $ toList (S.take 3 oneFiveS) @?= P.take 3 oneFive
  , testCase "drop" $ toList (S.drop 3 oneFiveS) @?= P.drop 3 oneFive
  , testCase "takeWhile" $ toList (S.takeWhile (/= 3) oneFiveS) @?= P.takeWhile (/= 3) oneFive
  , testCase "dropWhile" $ toList (S.dropWhile (/= 3) oneFiveS) @?= P.dropWhile (/= 3) oneFive
  , testCase "span" $ tupToList (S.span (/= 3) oneFiveS) @?= P.span (/= 3) oneFive
  , testCase "spanMaybe" $ tupToList (S.spanMaybe maybes) @?= (oneFive, map Just sixTen)
  , testCase "splitAt" $ tupToList (S.splitAt 2 oneFiveS) @?= P.splitAt 2 oneFive
  , testCase "groupEvery" $ S.groupEvery 2 long @?= grouped
  ]

folds = testGroup "Folds"
  [ transpose
  , intercalate
  ]

twoD :: Stream (Stream Int)
twoD = fromList
  [ fromList [ 11, 12, 13, 14, 15 ]
  , fromList [ 21, 22, 23, 24, 25 ]
  , fromList [ 31, 32, 33, 34, 35 ]
  , fromList [ 41, 42, 43, 44, 45 ]
  , fromList [ 51, 52, 53, 54, 55 ]
  ]

transposed :: [ Maybe Int ]
transposed =
  [ Just 11, Just 21, Just 31, Just 41, Just 51, Nothing
  , Just 12, Just 22, Just 32, Just 42, Just 52, Nothing
  , Just 13, Just 23, Just 33, Just 43, Just 53, Nothing
  , Just 14, Just 24, Just 34, Just 44, Just 54, Nothing
  , Just 15, Just 25, Just 35, Just 45, Just 55, Nothing
  ]

transpose = testCase "Transpose" $ toList (S.transpose twoD) @?= transposed

space :: [Int]
space = P.take 3 $ P.repeat 9
loopy :: [[Int]]
loopy = P.take 3 $ P.repeat oneFive
spaceS :: Stream Int
spaceS = S.take 3 $ S.repeat 9
loopyS :: Stream (Stream Int)
loopyS = S.take 3 $ S.repeat oneFiveS

intercalate = testCase "intercalate" $ (toList $ S.intercalate spaceS loopyS) @?= (L.intercalate space loopy)