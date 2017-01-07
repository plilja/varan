module Grammar 
    where

type Name = String

data Expr = Var Name 
            | Con Literal
            | Uno Unop Expr 
            | Duo Duop Expr Expr
            | MemberAccess Name Name 
            | FuncCall Name [Expr]
    deriving Show

data Literal = BoolLiteral Bool | StringLiteral String | IntLiteral Int | DoubleLiteral Double
    deriving Show

data Unop = Not deriving Show

data Duop = Add | Sub | Div | Mul | And | Or | Iff deriving Show

data Stmt = Nop  
            | StVd VarDecl
            | Name := Expr 
            | If Expr Stmt Stmt 
            | While Expr Stmt
            | Seq [Stmt] 
            | Func Name [VarDecl] String Stmt
            | StFuncCall Expr
            | Type Name [VarDecl]
    deriving Show

data VarDecl = Single Name String | Array Name String
    deriving Show
