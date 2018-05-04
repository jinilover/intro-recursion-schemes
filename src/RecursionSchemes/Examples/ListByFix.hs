{-# LANGUAGE DeriveFunctor #-}

module RecursionSchemes.Examples.ListByFix where

import RecursionSchemes.Examples.Types
import RecursionSchemes.Fix.Funcs
import Data.List.Ordered (merge)

mergeSortByListF :: Ord a => [a] -> [a]
mergeSortByListF = hylo mergeFromListF splitToListF
  where splitToListF [] = Nil
        splitToListF (x : xs) = Cons x xs
        mergeFromListF Nil = []
        mergeFromListF (Cons x xs) = merge [x] xs

data TreeF a r = Empty | Leaf a | Branch r r deriving Functor

mergeSortByTreeF :: Ord a => [a] -> [a]
mergeSortByTreeF = hylo mergeFromTreeF splitToTreeF
  where splitToTreeF [] = Empty
        splitToTreeF [x] = Leaf x
        splitToTreeF xs = uncurry Branch . splitAt (length xs `div` 2) $ xs
        mergeFromTreeF Empty = []
        mergeFromTreeF (Leaf x) = [x]
        mergeFromTreeF (Branch l r) = merge l r
