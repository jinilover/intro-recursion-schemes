{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module RecursionSchemes.Examples.NatByFix where

import RecursionSchemes.Examples.Types
import RecursionSchemes.Fix.Types
import RecursionSchemes.Fix.Funcs
import RecursionSchemes.Fixpoint.Types

type Nat = Fix NatF

factByNat :: Nat -> Int
factByNat = para alg
  where alg Zero = 1
        alg (Succ (a, nat)) = (cata inF . inF . Succ $ nat) * a

-- smart constructors
zero_ :: Nat
zero_ = Fix Zero

succ_ :: Nat -> Nat
succ_ = Fix . Succ
