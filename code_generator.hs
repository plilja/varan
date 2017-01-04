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
statementToCode (Func name vars ret body) = ret 
                                    ++ " " ++ name 
                                    ++ "(" ++ (varsToCode vars) ++ ") {\n" 
                                    ++ statementToCode body 
                                    ++ "\n}\n" 
statementToCode (FuncCall name params) = name ++ "(" 
                                        ++ (L.intercalate "," (map expressionToCode params)) 
                                        ++ ")"
statementToCode (Type name members) = "struct " ++ name ++ "{\n" 
                                        ++ (L.intercalate ";\n" (map varToCode members)) 
                                        ++ "\n}"
statementToCode (StVd var) = varToCode var

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

varsToCode vs = L.intercalate ", " $ map varToCode vs

varToCode :: VarDecl -> String
varToCode (Single name type_) = type_ ++ " " ++ name
varToCode (Array name type_) = type_ ++ " " ++ name ++ "[256]" -- TODO custom length

unopToCode :: Unop -> String
unopToCode Not = "!"

duopToCode :: Duop -> String
duopToCode And = "&&"
duopToCode Or = "||"
duopToCode Iff = "=="


