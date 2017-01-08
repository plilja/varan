module CodeGenerator where

import Grammar
import Util
import qualified Data.List as L
import qualified Data.Char as C


programToCode :: Stmt -> String
programToCode (Seq stmts) = fst $ divideIntoMain stmts
programToCode stmt = fst $ divideIntoMain [stmt]

mainToCode :: Stmt -> Maybe String
mainToCode (Seq stmts) = mainOrEmpty $ snd $ divideIntoMain stmts
mainToCode stmt = mainOrEmpty $ snd $ divideIntoMain [stmt]

divideIntoMain :: [Stmt] -> (String, String)
divideIntoMain [] = ("", "")
divideIntoMain ((Func name vars ret body):ss) = addLeft (statementToCode (Func name vars ret body)) (divideIntoMain ss)
divideIntoMain ((Type name vars):ss) = addLeft (statementToCode (Type name vars)) (divideIntoMain ss)
divideIntoMain (s:ss) = addRight (statementToCode s) (divideIntoMain ss)

addLeft :: String -> (String, String) -> (String, String)
addLeft s (a, b) = (s ++ a, b)

addRight :: String -> (String, String) -> (String, String)
addRight s (a, b) = (a, s ++ b)

mainOrEmpty :: String -> Maybe String
mainOrEmpty [] = Nothing 
mainOrEmpty xs = Just $ "int main() {\n" ++ (indent xs) ++ "}\n"

statementToCode :: Stmt -> String
statementToCode (Seq stmts) = concat $ map statementToCode stmts
statementToCode (If expr consequent) = "if (" ++ 
                                            expressionToCode expr ++
                                            ") {\n" ++
                                            indent (statementToCode consequent) ++
                                            "\n}\n"
statementToCode (IfElse expr consequent alternative) = "if (" ++ 
                                                    expressionToCode expr ++
                                                    ") {\n" ++
                                                    indent (statementToCode consequent) ++
                                                    "\n} else {" ++
                                                    indent (statementToCode alternative) ++
                                                    "\n}\n"
statementToCode (While expr stmt) = "while (" ++ expressionToCode expr ++ ") {\n" ++
                                    indent (statementToCode stmt) ++
                                    "\n}\n"
statementToCode (For initial cond increment body) = 
                                    statementToCode initial 
                                    ++ "for (" 
                                    ++ "; "
                                    ++ expressionToCode cond 
                                    ++ "; "
                                    ++ simpleStatementToCode increment 
                                    ++ ") {\n"
                                    ++ indent (statementToCode body)
                                    ++ "\n}\n"
statementToCode (Func name vars ret body) = (typeToCode ret)
                                    ++ " " ++ name 
                                    ++ "(" ++ (varsToCode vars) ++ ") {\n" 
                                    ++ indent (statementToCode body)
                                    ++ "}\n\n" 
statementToCode (Type name members) = "struct " ++ name ++ "{\n" 
                                        ++ indent (L.intercalate ";\n" (map varToCode members)) 
                                        ++ ";\n};\n\n"
statementToCode stmt = simpleStatementToCode stmt ++ ";\n" -- Default

-- | For statements that can be part of for instance a for expression
-- TODO consider cleaner solution, maybe make part of grammar
simpleStatementToCode :: Stmt -> String
simpleStatementToCode Nop = ""
simpleStatementToCode (variable := expr) = variable ++ " = (" ++ expressionToCode expr ++ ")"
simpleStatementToCode (StFuncCall funcCall) = expressionToCode funcCall
simpleStatementToCode (StVd var) = (varToCode var)


expressionToCode :: Expr -> String
expressionToCode (Var v) = v 
expressionToCode (MemberAccess v m) = v ++ "->" ++ m 
expressionToCode (Con l) = literalToCode l 
expressionToCode (Uno unop expr) = unopToCode unop ++ "(" ++ expressionToCode expr ++ ")"
expressionToCode (Duo duop expr1 expr2) = "(" ++ 
                                         expressionToCode expr1 ++
                                         ")" ++ 
                                         duopToCode duop ++
                                         "(" ++ 
                                         expressionToCode expr2 ++
                                         ")"
expressionToCode (FuncCall name params) = name ++ "(" 
                                        ++ (L.intercalate "," (map expressionToCode params)) 
                                        ++ ")"

literalToCode :: Literal -> String
literalToCode (BoolLiteral b) = map C.toLower $ show b 
literalToCode (IntLiteral i) = show i
literalToCode (DoubleLiteral d) = show d
literalToCode (StringLiteral s) = "makeString(" ++ show s ++ ")" -- TODO memory leak

varsToCode vs = L.intercalate ", " $ map varToCode vs

varToCode :: VarDecl -> String
varToCode (Single name type_) = (typeToCode type_) ++ " " ++ (if (isPrimitive type_) then name else "*" ++ name)
varToCode (Array name type_) = (typeToCode type_) ++ " " ++ name ++ "[256]" -- TODO custom length

typeToCode :: String -> String
typeToCode t | isPrimitive t = map C.toLower t
             | otherwise = "struct " ++ t

isPrimitive :: String -> Bool
isPrimitive t = t `elem` ["Int", "Void", "Double", "Bool", "Char"]

unopToCode :: Unop -> String
unopToCode Not = "!"

duopToCode :: Duop -> String
duopToCode And = "&&"
duopToCode Or = "||"
duopToCode Iff = "=="
duopToCode Mul = "*"
duopToCode Div = "/"
duopToCode Add = "+"
duopToCode Sub = "-"
duopToCode Mod = "%"
duopToCode Lt = "<"
duopToCode LtEq = "<="
duopToCode Gt = ">"
duopToCode GtEq = ">="

