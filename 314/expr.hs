data Expr = Val Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show)

--should be complete
evalExpr :: Expr -> Int
evalExpr (Val i) = i
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 `div` evalExpr e2

exprFact :: Expr -> Expr
exprFact (Val 0) = Val (1)
exprFact v@(Val n) = Mul v ((exprFact . Val . evalExpr) (Sub v (Val 1)))
