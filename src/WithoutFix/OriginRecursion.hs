module WithoutFix.OriginRecursion where

import Control.Monad (join)
import Control.Arrow ((***))
import Data.List.Ordered (merge)

mergeSort :: Ord a => [a] -> [a]
mergeSort = uncurry merge . splitList
  where splitList xs
          | length xs < 2 = (xs, [])
          | otherwise = join (***) mergeSort . splitAt (length xs `div` 2) $ xs

factorial :: Int -> Int
factorial n
  | n < 2 = 0
  | otherwise = n * factorial (n - 1)

data Expr = Const Int
           | Add Expr Expr
           | Mul Expr Expr

eval :: Expr -> Int
eval (Const x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 + eval e2
