module ListCheck
     ( mergeSortCheck )
      where

import Test.QuickCheck
import Test.Hspec
import Data.List (sort)
import WithoutFix.OriginRecursion (mergeSort)
import RecursionSchemes.Examples.ListByFix (mergeSortByListF, mergeSortByTreeF)

mergeSortCheck :: Spec
mergeSortCheck =
  describe "different merge sorting" $ do
    it "mergeSort' is always correct" $
      forAll (genList :: Gen [Int]) $ \list -> sort list == mergeSort list
    it "mergeSortByListF is always correct" $
      forAll (genList :: Gen [Int]) $ \list -> sort list == mergeSortByListF list
    it "mergeSortByTreeF is always correct" $
      forAll (genList :: Gen [Int]) $ \list -> sort list == mergeSortByTreeF list

genList :: Arbitrary a => Gen [a]
genList = sized $ \n -> do
  listSize <- choose (0, n)
  mapM (const arbitrary) [1 .. listSize]
