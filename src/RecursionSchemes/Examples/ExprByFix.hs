module RecursionSchemes.Examples.ExprByFix where

import RecursionSchemes.Examples.Types
import RecursionSchemes.Fix.Funcs
import RecursionSchemes.Fix.Types
import Data.Maybe
import Data.Monoid
import Control.Arrow

type Expr = Fix ExprF

eval :: ExprF Int -> Int
eval (Const c) = c
eval (Add x y) = x + y
eval (Mul x y) = x * y

prettyFormat :: ExprF String -> String
prettyFormat (Const c) = show c
prettyFormat (Add s1 s2) = "(" <> s1 <> " + " <> s2 <> ")"
prettyFormat (Mul s1 s2) = s1 <> " * " <> s2

e1 :: Expr
e1 = mul
      ( mul
        (add (constInt 3) (constInt 0))
        (add (constInt 3) (constInt 2))
      )
      (constInt 3)

evalExpr :: Int
evalExpr = cata eval e1

formatExpr :: String
formatExpr = cata prettyFormat e1

-- smart constructors
constInt :: Int -> Expr
constInt = Fix . Const

add :: Expr -> Expr -> Expr
add e = Fix . Add e

mul :: Expr -> Expr -> Expr
mul e = Fix . Mul e
