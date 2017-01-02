module Grammar 
    where

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr
    deriving Show
data Unop = Not deriving Show
data Duop = And | Or | Iff deriving Show
data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
          | Seq [Stmt]
    deriving Show
