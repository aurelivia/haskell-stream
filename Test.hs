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
  [ constructors
  , semigroup
  , functor
  , applicative
  , monad
  , accessors
  , slices
  , predicates
  , folds
  , maps
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
-- oneFiveS = fromList oneFive
oneFiveS = (fromList [ 1, 2 ]) <> S.Skip id (fromList [ 3, 4 ]) <> S.Skip id (pure 5)

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

constructors = testGroup "Constructors"
  [ testCase "Cons" $ (toList $ S.cons 0 oneFiveS) @?= (0 : oneFive)
  , testCase "Snoc" $ (toList $ S.snoc oneFiveS 6) @?= (oneFive <> [6])
  , testCase "To/From Lists" $ (toList . toStream) oneFive @?= oneFive
  , testCase "To/From Sequence" $ (S.toSeq . S.fromSeq) oneFiveSeq @?= oneFiveSeq
  , testCase "Repeat" $ (P.take 10 $ toList $ S.repeat 1) @?= (P.take 10 $ P.repeat 1)
  , testCase "Repeat'" $ (S.repeat' 5 1) @?= (toStream [ 1, 1, 1, 1, 1 ])
  , testCase "Cycle" $ (P.take 10 $ toList $ S.cycle oneFiveS) @?= (P.take 10 $ P.cycle oneFive)
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

accessors = testGroup "Accessors"
 [ testCase "Head" $ (S.head oneFiveS) @?= Just 1
 , testCase "Head Empty" $ (S.head done) @?= Nothing
 , testCase "HeadElse" $ (S.headElse 0 oneFiveS) @?= 1
 , testCase "HeadElse Empty" $ (S.headElse 0 done) @?= 0
 , testCase "Last" $ (S.last oneFiveS) @?= Just 5
 , testCase "Last Empty" $ (S.last done) @?= Nothing
 , testCase "LastElse" $ (S.lastElse 0 oneFiveS) @?= 5
 , testCase "LastElse Empty" $ (S.lastElse 0 done) @?= 0
 , testCase "Uncons" $ (S.uncons oneFiveS) @?= Just (1, fromList [ 2, 3, 4, 5 ])
 , testCase "Uncons Empty" $ (S.uncons done) @?= Nothing
 , testCase "!? Within" $ (oneFiveS S.!? 2) @?= Just 3
 , testCase "!? Below" $ (oneFiveS S.!? (-5)) @?= Nothing
 , testCase "!? Above" $ (oneFiveS S.!? 25) @?= Nothing
 , testCase "!? Empty" $ (done S.!? 2) @?= Nothing
 , testCase "Length" $ (S.length oneFiveS) @?= 5
 , testCase "Length Empty" $ (S.length done) @?= 0
 , testCase "Done" $ (S.done oneFiveS) @?= False
 , testCase "Done Empty" $ (S.done done) @?= True
 ]

sliceList s e = P.take (e - s) . P.drop s
tupToList (x,y) = (toList x, toList y)

slices = testGroup "Slices"
  [ testCase "tail" $ toList (S.tail oneFiveS) @?= P.tail oneFive
  , testCase "init" $ toList (S.init oneFiveS) @?= P.init oneFive
  , testCase "take" $ toList (S.take 3 oneFiveS) @?= P.take 3 oneFive
  , testCase "drop" $ toList (S.drop 3 oneFiveS) @?= P.drop 3 oneFive
  , testCase "slice" $ toList (S.slice 2 2 oneFiveS) @?= sliceList 2 2 oneFive
  , testCase "takeWhile" $ toList (S.takeWhile (/= 3) oneFiveS) @?= P.takeWhile (/= 3) oneFive
  , testCase "dropWhile" $ toList (S.dropWhile (/= 3) oneFiveS) @?= P.dropWhile (/= 3) oneFive
  , testCase "span" $ tupToList (S.span (/= 3) oneFiveS) @?= P.span (/= 3) oneFive
  ]

isEven :: Int -> Bool
isEven x = x `rem` 2 == 0

predicates = testGroup "Predicates"
  [ testCase "Equality Done" $ (done == done) @?= True
  , testCase "Equality Singleton" $ (oneFiveS == done) @?= False
  , testCase "Equality Equals" $ (oneFiveS == oneFiveS) @?= True
  , testCase "Elem True" $ (S.elem 3 oneFiveS) @?= True
  , testCase "Elem False" $ (S.elem 7 oneFiveS) @?= False
  , testCase "Filter" $ (S.filter isEven oneFiveS) @?= fromList [ 2, 4 ]
  , testCase "Partition" $ (S.partition isEven oneFiveS) @?= (fromList [ 2, 4 ], fromList [ 1, 3, 5 ])
  ]

folds = testGroup "Folds"
  [ testCase "foldr" $ foldr (\x xs -> show x ++ xs) "" oneFiveS @?= "12345"
  , testCase "foldr'" $ foldr' (\x xs -> show x ++ xs) "" oneFiveS @?= "12345"
  , testCase "foldl" $ foldl (\xs x -> show x ++ xs) "" oneFiveS @?= "54321"
  , testCase "foldl'" $ foldl' (\xs x -> show x ++ xs) "" oneFiveS @?= "54321"
  ]

heads = fromList [ 11, 21, 31, 41, 51 ]

tails :: Stream (Stream Int)
tails = fromList
  [ fromList [ 12, 13, 14, 15 ]
  , fromList [ 22, 23, 24, 25 ]
  , fromList [ 32, 33, 34, 35 ]
  , fromList [ 42, 43, 44, 45 ]
  , fromList [ 52, 53, 54, 55 ]
  ]

maps = testGroup "Maps"
  [ testCase "Heads" $ (S.heads twoD) @?= heads
  , testCase "Tails" $ (S.tails twoD) @?= tails
  , testCase "intersperse" $ (toList $ S.intersperse 1 oneFiveS) @?= (L.intersperse 1 oneFive)
  , intercalate
  , transpose
  , testCase "padLeft" $ (toList $ S.padLeft 5 0 oneFiveS) @?= [ 0, 0, 0, 0, 0, 1, 2, 3, 4, 5 ]
  , testCase "padRight" $ (toList $ S.padRight 5 0 oneFiveS) @?= [ 1, 2, 3, 4, 5, 0, 0, 0, 0, 0 ]
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

twoD :: Stream (Stream Int)
twoD = fromList
  [ fromList [ 11, 12, 13, 14, 15 ]
  , fromList [ 21, 22, 23, 24, 25 ]
  , fromList [ 31, 32, 33, 34, 35 ]
  , mempty
  , fromList [ 41, 42, 43, 44, 45 ]
  , fromList [ 51, 52, 53, 54, 55 ]
  ]

transposed :: Stream (Stream Int)
transposed = fromList
  [ fromList [ 11, 21, 31, 41, 51 ]
  , fromList [ 12, 22, 32, 42, 52 ]
  , fromList [ 13, 23, 33, 43, 53 ]
  , fromList [ 14, 24, 34, 44, 54 ]
  , fromList [ 15, 25, 35, 45, 55 ]
  ]

transpose = testCase "Transpose" $ S.transpose twoD @?= transposed