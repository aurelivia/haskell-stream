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

main = defaultMain $ testGroup "Stream"
  [ conversions
  , predicates
  , functor
  , applicative
  ]

done :: Stream Int
done = S.Done

{-# INLINE oneFive #-}
oneFive :: [Int]
oneFive = [ 1, 2, 3, 4, 5 ]

{-# INLINE oneFiveS #-}
oneFiveS :: Stream Int
oneFiveS = fromList oneFive

{-# INLINE oneFiveSeq #-}
oneFiveSeq :: Seq Int
oneFiveSeq = fromList oneFive

{-# INLINE toStream #-}
toStream :: [a] -> Stream a
toStream = fromList

conversions = testGroup "Conversions"
  [ testCase "To/From Lists" $ (toList . toStream) oneFive @?= oneFive
  , testCase "To/From Sequence" $ (S.toSeq . S.fromSeq) oneFiveSeq @?= oneFiveSeq
  ]

predicates = testGroup "Predicates"
  [ testCase "Equality: Done" $ (done == done) @?= True
  , testCase "Equality: Singleton" $ (oneFiveS == done) @?= False
  , testCase "Equality: Equals" $ (oneFiveS == oneFiveS) @?= True
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