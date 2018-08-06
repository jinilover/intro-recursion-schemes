module RecursionSchemes.Fix.Types
    ( Fix (..))
    where

newtype Fix f = Fix { unFix :: f (Fix f) }
