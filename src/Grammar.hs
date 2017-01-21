module Grammar 
    where

type Name = String

data Expr = Var Name 
            | Con Literal
            | Uno Unop Expr 
            | Duo Duop Expr Expr
            | MemberAccess Name Name 
            | FuncCall Name [Expr]
            | New Name
    deriving Show

data Literal = BoolLiteral Bool | StringLiteral String | IntLiteral Int | DoubleLiteral Double
    deriving Show

data Unop = Not deriving Show

data Duop = Add | Sub | Div | Mul | Mod | And | Or | Iff | Lt | Gt | LtEq | GtEq deriving Show

data Stmt = Nop  
            | StVd VarDecl
            | Expr := Expr 
            | If Expr Stmt Stmt 
            | For Stmt Expr Stmt Stmt
            | Seq [Stmt] 
            | Func Name [VarDecl] String Stmt
            | StFuncCall Expr
            | Type Name [VarDecl]
    deriving Show

data VarDecl = Single Name String | Array Name String
    deriving Show
