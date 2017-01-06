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
statementToCode Nop = ""
statementToCode (variable := expr) = variable ++ " = (" ++ expressionToCode expr ++ ");\n"
statementToCode (If expr consequent alternative) = "if (" ++ 
                                                    expressionToCode expr ++
                                                    ") {\n" ++
                                                    indent (statementToCode consequent) ++
                                                    "\n} else {" ++
                                                    indent (statementToCode alternative) ++
                                                    "\n}\n"
statementToCode (While expr stmt) = "while (" ++ expressionToCode expr ++ ") {\n" ++
                                    indent (statementToCode stmt) ++
                                    "\n}\n"
statementToCode (Seq stmts) = concat $ map statementToCode stmts
statementToCode (Func name vars ret body) = (typeToCode ret)
                                    ++ " " ++ name 
                                    ++ "(" ++ (varsToCode vars) ++ ") {\n" 
                                    ++ indent (statementToCode body)
                                    ++ "}\n\n" 
statementToCode (FuncCall name params) = name ++ "(" 
                                        ++ (L.intercalate "," (map expressionToCode params)) 
                                        ++ ");\n"
statementToCode (Type name members) = "struct " ++ name ++ "{\n" 
                                        ++ indent (L.intercalate ";\n" (map varToCode members)) 
                                        ++ ";\n};\n\n"
statementToCode (StVd var) = (varToCode var) ++ ";\n"

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

