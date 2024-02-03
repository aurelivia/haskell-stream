{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Stream
import GHC.Exts (toList, fromList)

main = defaultMain $ testGroup "Range"
  [ toFromList
  ]

oneFive :: [Int]
oneFive = [ 1, 2, 3, 4, 5 ]

toFromList = testCase "To/From Lists" $ (toList . fromList @(Stream Int)) oneFive @?= oneFive
