{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module RecursionSchemes.Fixpoint.Types
    ( Fixpoint (..)
    ) where

import RecursionSchemes.Fix.Types

class Functor f => Fixpoint f t | t -> f where
  inF :: f t -> t
  outF :: t -> f t

instance Functor f => Fixpoint f (Fix f) where
  inF = Fix
  outF = unFix
