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
