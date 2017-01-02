module CodeGenerator
    where

import Grammar
import qualified Data.List as L

statementToCode :: Stmt -> String
statementToCode Nop = ""
statementToCode (variable := expr) = variable ++ " = (" ++ expressionToCode expr ++ ")"
statementToCode (If expr consequent alternative) = "if (" ++ 
                                                    expressionToCode expr ++
                                                    ") {\n" ++
                                                    statementToCode consequent ++
                                                    "\n} else {" ++
                                                    statementToCode alternative ++
                                                    "\n}\n"
statementToCode (While expr stmt) = "while (" ++ expressionToCode expr ++ ") {\n" ++
                                    statementToCode stmt ++
                                    "\n}\n"
statementToCode (Seq stmts) = (L.intercalate ";\n" $ map statementToCode stmts) ++ ";"


expressionToCode :: Expr -> String
expressionToCode (Var v) = v 
expressionToCode (Con b) = show b 
expressionToCode (Uno unop expr) = unopToCode unop ++ "(" ++ expressionToCode expr ++ ")"
expressionToCode (Duo duop expr1 expr2) = "(" ++ 
                                         expressionToCode expr1 ++
                                         ")" ++ 
                                         duopToCode duop ++
                                         "(" ++ 
                                         expressionToCode expr2 ++
                                         ")"

unopToCode :: Unop -> String
unopToCode Not = "!"

duopToCode :: Duop -> String
duopToCode And = "&&"
duopToCode Or = "||"
duopToCode Iff = "=="


