{-# LANGUAGE MultiParamTypeClasses #-}

module RecursionSchemes.Examples.ListByFixpoint where

import RecursionSchemes.Examples.Types
import RecursionSchemes.Fixpoint.Funcs
import RecursionSchemes.Fixpoint.Types

instance Fixpoint (ListF a) [a] where
  inF Nil = []
  inF (Cons x xs) = x : xs
  outF [] = Nil
  outF (x : xs) = Cons x xs

slidingByFixpoint :: Int -> [a] -> [[a]]
slidingByFixpoint n = para alg
  where alg Nil = []
        alg (Cons x (r, xs)) = take n (x:xs) : r
