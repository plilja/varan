module Parser
    where

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
         --     , identStart = letter
         --     , identLetter = alphaNum
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedOpNames = ["~", "&&", "==", ":="]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od", "func", "::"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , comma = m_comma
           , commaSep = m_commaSep
           , braces = m_braces
           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef

program :: Parser Stmt
program = m_whiteSpace >> statements <* eof

statements :: Parser Stmt
statements = fmap Seq (m_semiSep1 statement)

statement :: Parser Stmt
statement = (m_reserved "nop" >> return Nop)
      <|> funcCall
      <|> assignment 
      <|> ifelse 
      <|> while 
      <|> func

expression :: Parser Expr
expression = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "||" >> return (Duo Or)) AssocLeft]
        , [Infix (m_reservedOp "==" >> return (Duo Iff)) AssocLeft]
        ]

term :: Parser Expr
term = m_parens expression
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))

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
    t <- typedecl
    body <- m_braces statement
    return (Func name ps t body)

func_params :: Parser [VarDecl]
func_params = m_commaSep varDecl

funcCall :: Parser Stmt
funcCall = do
    name <- m_identifier
    params <- m_parens (m_commaSep expression)
    return (FuncCall name params)

varDecl :: Parser VarDecl
varDecl = do
    n <- m_identifier
    m_reserved "::"
    t <- typedecl
    return (Vd n t)

typedecl :: Parser String
typedecl = m_identifier
    




