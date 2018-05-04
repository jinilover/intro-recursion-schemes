module RecursionSchemes.Fixpoint.Funcs
    ( para
    ) where

import RecursionSchemes.Fixpoint.Types
import Control.Arrow

para :: Fixpoint f t => (f (a, t) -> a) -> t -> a
para alg = alg . fmap (para alg &&& id) . outF
