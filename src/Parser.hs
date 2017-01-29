module Parser where

import Grammar
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

languageDef = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedOpNames = ["~", "&&", "==", ":="]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", 
                                 "while", "func", "::", 
                                 "type", "new", "return",
                                 "continue"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , integer = m_integer
           , stringLiteral = m_stringLiteral
           , float = m_float
           , comma = m_comma
           , commaSep = m_commaSep
           , braces = m_braces
           , brackets = m_brackets
           , semi = m_semi
           , dot = m_dot
           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef

program :: Parser Stmt
program = m_whiteSpace >> statements <* eof

statements :: Parser Stmt
statements = fmap Seq $ many statement

statement :: Parser Stmt
statement = 
    try (inlineStatement <* m_semi)
    <|> try ifelse
    <|> try if_
    <|> try for
    <|> try while
    <|> try func
    <|> try continue
    <|> try return_
    <|> typedecl

semsep :: Parser a -> Parser [a]
semsep p = do
    a <- p
    m_semi
    rem <- semsep p <|> return []
    return (a:rem)

inlineStatement :: Parser Stmt
inlineStatement = (m_reserved "nop" >> return Nop)
      <|> try stFuncCall
      <|> try stVarDeclAndAssignment
      <|> try stVarDecl
      <|> assignment 

expression :: Parser Expr
expression = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "*" >> return (Duo Mul)) AssocLeft]
        , [Infix (m_reservedOp "/" >> return (Duo Div)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Sub)) AssocLeft]
        , [Infix (m_reservedOp "%" >> return (Duo Mod)) AssocLeft]
        , [Infix (m_reservedOp "<" >> return (Duo Lt)) AssocLeft]
        , [Infix (m_reservedOp "<=" >> return (Duo LtEq)) AssocLeft]
        , [Infix (m_reservedOp ">" >> return (Duo Gt)) AssocLeft]
        , [Infix (m_reservedOp ">=" >> return (Duo GtEq)) AssocLeft]
        , [Infix (m_reservedOp "&&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "||" >> return (Duo Or)) AssocLeft]
        , [Infix (m_reservedOp "==" >> return (Duo Iff)) AssocLeft]
        ]

term :: Parser Expr
term = m_parens expression
       <|> try memberAccess 
       <|> try funcCall
       <|> try variable 
       <|> try new 
       <|> fmap Con literal

literal :: Parser Literal
literal = (m_reserved "true" >> return (BoolLiteral True))
       <|> (m_reserved "false" >> return (BoolLiteral False))
       <|> try (fmap (\v -> IntLiteral (fromInteger v)) m_integer)
       <|> try (fmap (\v -> DoubleLiteral v) m_float)
       <|> fmap StringLiteral m_stringLiteral

variable :: Parser Expr
variable = fmap Var m_identifier

memberAccess :: Parser Expr
memberAccess = do
    v <- m_identifier
    m_dot
    m <- m_identifier
    return (MemberAccess v m)

new :: Parser Expr
new = do
    m_reserved "new"
    t <- m_identifier
    return (New t)

assignment :: Parser Stmt
assignment = do 
    v <- (try memberAccess <|> variable)
    m_reservedOp ":="
    e <- expression
    return (v := e)

ifelse :: Parser Stmt
ifelse = do 
    m_reserved "if"
    b <- m_parens expression
    consequent <- m_braces statements
    m_reserved "else"
    alternative <- try ifelse <|> (m_braces statements)
    return (If b consequent alternative)

if_ :: Parser Stmt
if_ = do 
    m_reserved "if"
    b <- m_parens expression
    consequent <- m_braces statements
    return (If b consequent Nop)

for :: Parser Stmt
for = do
    m_reserved "for"
    (initial, cond, inc) <- m_parens $ do
        initial <- inlineStatement
        m_semi
        cond <- expression
        m_semi
        inc <- inlineStatement
        return (initial, cond, inc)
    body <- m_braces statements
    return (For initial cond inc body)

while :: Parser Stmt
while = do 
    m_reserved "while"
    cond <- m_parens expression
    body <- m_braces statements
    return (For Nop cond Nop body)

return_ :: Parser Stmt
return_ = do
    m_reserved "return"
    expr <- expression
    m_semi
    return (Return expr)

continue :: Parser Stmt
continue = do
    m_reserved "continue"
    m_semi
    return Continue

func :: Parser Stmt
func = do
    m_reserved "func"
    name <- m_identifier
    ps <- m_parens func_params
    m_reserved "::"
    t <- m_identifier
    body <- m_braces statements
    return (Func name ps t body)

func_params :: Parser [VarDecl]
func_params = m_commaSep varDecl

stFuncCall :: Parser Stmt
stFuncCall = fmap StFuncCall funcCall

funcCall :: Parser Expr
funcCall = do
    name <- m_identifier
    params <- m_parens (m_commaSep expression)
    return (FuncCall name params)

stVarDecl :: Parser Stmt
stVarDecl = do
    vd <- varDecl
    return (StVd vd)

stVarDeclAndAssignment :: Parser Stmt
stVarDeclAndAssignment = do
    vd <- varDecl
    m_reserved ":="
    e <- expression
    return (Seq [StVd vd, Var (varName vd) := e])

varName :: VarDecl -> String
varName (Single n _) = n
varName (Array n _) = n

varDecl :: Parser VarDecl
varDecl = try arrayDecl <|> singleDecl
    where
        arrayDecl :: Parser VarDecl
        arrayDecl = do
            n <- m_identifier
            m_reserved "::"
            t <- m_identifier
            m_brackets m_whiteSpace
            return (Array n t)
        singleDecl :: Parser VarDecl
        singleDecl = do
            n <- m_identifier
            m_reserved "::"
            t <- m_identifier
            return (Single n t)

typedecl :: Parser Stmt
typedecl = do
    m_reserved "type"
    name <- m_identifier
    members <- m_braces (semsep varDecl)
    return (Type name members)


