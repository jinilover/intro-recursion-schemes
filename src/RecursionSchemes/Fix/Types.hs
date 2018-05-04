module RecursionSchemes.Fix.Types
    ( Fix (Fix, unFix))
    where

newtype Fix f = Fix { unFix :: f (Fix f) }
