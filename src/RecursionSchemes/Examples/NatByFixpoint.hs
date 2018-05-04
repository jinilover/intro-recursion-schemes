{-# LANGUAGE FlexibleContexts #-}

module RecursionSchemes.Examples.NatByFixpoint where

import RecursionSchemes.Examples.Types
import RecursionSchemes.Fixpoint.Types
import RecursionSchemes.Fixpoint.Funcs

factByInt :: Int -> Int
factByInt = para alg
  where alg Zero = 1
        alg (Succ (a, n)) = (inF . Succ $ n) * a
