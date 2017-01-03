module Grammar 
    where

type Name = String

data Expr = Var Name 
            | Con Bool
            | Uno Unop Expr 
            | Duo Duop Expr Expr
    deriving Show

data Unop = Not deriving Show

data Duop = And | Or | Iff deriving Show

data Stmt = Nop  
            | StVd VarDecl
            | Name := Expr 
            | If Expr Stmt Stmt 
            | While Expr Stmt
            | Seq [Stmt] 
            | Func Name [VarDecl] String Stmt
            | FuncCall Name [Expr]
            | Type Name [VarDecl]
    deriving Show

data VarDecl = Vd Name String deriving Show

--data Type = Int_ | String_ | Double_ deriving Show
-- TODO user defined types
