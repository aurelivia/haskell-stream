{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import Prelude hiding (head, tail, intercalate)
import qualified Prelude as P
import qualified Data.List as L
import Data.Stream (Stream)
import qualified Data.Stream as S
import GHC.Exts (toList, fromList)
import Data.Sequence (Seq(..), ViewR(..), (<|))
import qualified Data.Sequence as Seq
import Data.Foldable (foldr, foldr', foldl, foldl')

main = defaultMain $ testGroup "Stream"
  [ conversions
  , predicates
  , semigroup
  , functor
  , applicative
  , monad
  , foldable
  ]

done :: Stream Int
done = S.Done

{-# INLINE toStream #-}
toStream :: [a] -> Stream a
toStream = fromList

{-# INLINE oneFive #-}
oneFive :: [Int]
oneFive = [ 1, 2, 3, 4, 5 ]

{-# INLINE oneFiveS #-}
oneFiveS :: Stream Int
oneFiveS = fromList oneFive

{-# INLINE oneFiveSeq #-}
oneFiveSeq :: Seq Int
oneFiveSeq = fromList oneFive

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

conversions = testGroup "Conversions"
  [ testCase "To/From Lists" $ (toList . toStream) oneFive @?= oneFive
  , testCase "To/From Sequence" $ (S.toSeq . S.fromSeq) oneFiveSeq @?= oneFiveSeq
  ]

predicates = testGroup "Predicates"
  [ testCase "Equality: Done" $ (done == done) @?= True
  , testCase "Equality: Singleton" $ (oneFiveS == done) @?= False
  , testCase "Equality: Equals" $ (oneFiveS == oneFiveS) @?= True
  ]

semigroup = testGroup "Semigroup"
  [ testCase "Two Empty" $ (toList $ (toStream []) <> (toStream @[Int] [])) @?= []
  , testCase "Left Full" $ (toList $ oneFiveS <> (toStream [])) @?= oneFive
  , testCase "Rght Full" $ (toList $ (toStream []) <> oneFiveS) @?= oneFive
  , testCase "Both Full" $ (toList $ oneFiveS <> oneFiveS) @?= oneFive ++ oneFive
  , testCase "Associativity" $ (oneFiveS <> (sixTenS <> elvFifS)) @?= ((oneFiveS <> sixTenS) <> elvFifS)
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
  [ testCase "Pure" $ toList (pure @Stream 1) @?= [ 1 ]
  , testCase "Identity" $ (toList $ pure id <*> oneFiveS) @?= oneFive
  , testCase "Composition" $ (pure (.) <*> timesTwo <*> timesFour <*> oneFiveS) @?= (timesTwo <*> (timesFour <*> oneFiveS))
  , testCase "Homomorphism" $ (timesTwo <*> pure @Stream 2) @?= (pure @Stream 4)
  , testCase "Interchange" $ (timesTwo <*> pure @Stream 2) @?= (pure ($ 2) <*> timesTwo)
  , testCase "Functor" $ (fmap ((*) 2) oneFiveS) @?= (pure ((*) 2) <*> oneFiveS)
  ]

repeatFiveTimes :: Int -> Stream Int
repeatFiveTimes x = fromList [ x, x, x, x, x ]
-- repeatFiveTimes = S.take 5 . S.repeat

fiveOnes :: Stream Int
fiveOnes = repeatFiveTimes 1

doubleDouble :: Int -> Stream Int
doubleDouble x = fromList [ x * 2, x * 2 ]

monad = testGroup "Monad"
  [ testCase "Return" $ (return @Stream 1) @?= (pure @Stream 1)
  , testCase "Left Identity" $ (return @Stream 1 >>= repeatFiveTimes) @?= fiveOnes
  , testCase "Right Identity" $ (fiveOnes >>= return) @?= fiveOnes
  , testCase "Associativity" $ (pure @Stream 1 >>= (\x -> repeatFiveTimes x >>= doubleDouble)) @?= ((pure @Stream 1 >>= repeatFiveTimes) >>= doubleDouble)
  , testCase "Functor" $ (fmap (* 2) oneFiveS) @?= (oneFiveS >>= return . (* 2))
  , testCase "Applicative" $ (timesTwo <*> oneFiveS) @?= (timesTwo >>= (\x -> oneFiveS >>= (\y -> return (x y))))
  , testCase "Pointless" $ (oneFiveS >> sixTenS) @?= (oneFiveS *> sixTenS)
  ]

foldable = testGroup "Foldable"
  [ testCase "foldr" $ foldr (\x xs -> show x ++ xs) "" oneFiveS @?= "12345"
  , testCase "foldr'" $ foldr' (\x xs -> show x ++ xs) "" oneFiveS @?= "12345"
  , testCase "foldl" $ foldl (\xs x -> show x ++ xs) "" oneFiveS @?= "54321"
  , testCase "foldl'" $ foldl' (\xs x -> show x ++ xs) "" oneFiveS @?= "54321"
  ]