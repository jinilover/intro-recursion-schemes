module RecursionSchemes.Fix.Funcs
      ( cata
      , ana
      , para
      , hylo) where

import RecursionSchemes.Fix.Types
import Control.Arrow

cata :: Functor f => (f a -> a) -> Fix f -> a
-- cata alg = alg . fmap (cata alg) . unFix
cata alg = hylo alg unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
-- ana coalg = Fix . fmap (ana coalg) . coalg
ana = hylo Fix

para :: Functor f => (f (a, Fix f) -> a) -> Fix f -> a
para alg = alg . fmap (para alg &&& id) . unFix

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
-- hylo alg coalg = cata alg . ana coalg
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg
