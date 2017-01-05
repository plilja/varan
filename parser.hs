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
                                 "if", "then", "else", "fi",
                                 "while", "do", "od", "func", 
                                 "::", "type"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           -- , semiSep1 = m_semiSep1
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
program = m_whiteSpace >> statement <* eof

statement :: Parser Stmt
statement = fmap Seq (semsep singleStatement)

semsep :: Parser a -> Parser [a]
semsep p = do
    a <- p
    m_semi
    rem <- semsep p <|> return []
    return (a:rem)

singleStatement :: Parser Stmt
singleStatement = (m_reserved "nop" >> return Nop)
      <|> try funcCall
      <|> try stVarDecl
      <|> try assignment 
      <|> try ifelse 
      <|> try while 
      <|> func
      <|> typedecl

expression :: Parser Expr
expression = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "||" >> return (Duo Or)) AssocLeft]
        , [Infix (m_reservedOp "==" >> return (Duo Iff)) AssocLeft]
        ]

term :: Parser Expr
term = m_parens expression
       <|> try memberAccess 
       <|> try (fmap Var m_identifier)
       <|> fmap Con literal



literal :: Parser Literal
literal = (m_reserved "true" >> return (BoolLiteral True))
       <|> (m_reserved "false" >> return (BoolLiteral False))
       <|> try (fmap (\v -> IntLiteral (fromInteger v)) m_integer)
       <|> try (fmap (\v -> DoubleLiteral v) m_float)
       <|> fmap StringLiteral m_stringLiteral

memberAccess :: Parser Expr
memberAccess = do
    v <- m_identifier
    m_dot
    m <- m_identifier
    return (MemberAccess v m)

assignment :: Parser Stmt
assignment = do 
    v <- m_identifier
    m_reservedOp ":="
    e <- expression
    return (v := e)

ifelse :: Parser Stmt
ifelse = do 
    m_reserved "if"
    b <- expression
    m_reserved "then"
    p <- statement
    m_reserved "else"
    q <- statement
    m_reserved "fi"
    return (If b p q)

while :: Parser Stmt
while = do 
    m_reserved "while"
    b <- expression
    m_reserved "do"
    p <- statement
    m_reserved "od"
    return (While b p)

func :: Parser Stmt
func = do
    m_reserved "func"
    name <- m_identifier
    ps <- m_parens func_params
    m_reserved "::"
    t <- m_identifier
    body <- m_braces statement
    return (Func name ps t body)

func_params :: Parser [VarDecl]
func_params = m_commaSep varDecl

funcCall :: Parser Stmt
funcCall = do
    name <- m_identifier
    params <- m_parens (m_commaSep expression)
    return (FuncCall name params)

stVarDecl :: Parser Stmt
stVarDecl = do
    vd <- varDecl
    return (StVd vd)

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
    -- m_semi
    return (Type name members)


