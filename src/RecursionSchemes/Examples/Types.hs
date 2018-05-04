{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RecursionSchemes.Examples.Types where

import RecursionSchemes.Fixpoint.Types

data ListF a r = Nil | Cons a r deriving Functor

data ExprF r = Const Int
             | Add r r
             | Mul r r
             deriving ( Show, Eq, Ord, Functor, Foldable, Traversable )

data NatF r = Zero | Succ r deriving Functor
-- N.B.
-- instance Functor Nat where
--   fmap _ Zero = Zero
--   fmap f (Succ r) = Succ (f r)

instance Fixpoint NatF Int where
  inF Zero = 0
  inF (Succ r) = r + 1
  outF 0 = Zero
  outF n
    | n > 0 = Succ (n - 1)
    | otherwise = Zero
