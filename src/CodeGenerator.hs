{-# LANGUAGE TemplateHaskell #-}

module CodeGenerator where

import Grammar
import Util
import OsFuncs
import Control.Lens
import qualified Data.List as L
import qualified Data.Char as C
import Text.Printf

data Env = Env {
    _functions :: [Stmt],
    -- TODO types
    _variables :: [VarDecl],
    _code :: Code
}

data Code = Code {
   _header :: String, 
   _source :: String 
}

makeLenses ''Env
makeLenses ''Code

modulesToCode :: [Module] -> [(Module, Code)]
modulesToCode modules = map (\m -> (m, go modules m)) modules
    where
        go :: [Module] -> Module -> Code
        go ms m@(Module name (Seq stmts)) = let includeStd = if name == "varan_std" then [] else ["varan_std"]
                                                importNames = (includeStd ++)  $ map (\(Import name) -> name) $ filter isImport stmts
                                                imports = map (getModule ms) $ importNames
                                                importedStatements = concat $ map (\(Module _ (Seq stmts)) -> stmts) imports
                                                importedFunctions = filter isFunc importedStatements
                                                --funcs = (++ importedFunctions) $ filter isFunc stmts
                                                result = topLevelStatementToCode (over functions (++ importedFunctions) emptyEnv) $ filter (\s -> not (isImport s)) stmts
                                                importsAsCode = concat $ map (\i -> "#include \"" ++ i ++ ".h\"\n") importNames
                                             in Code (importsAsCode ++ (result^.code.header)) (importsAsCode ++ (result^.code.source))
        getModule :: [Module] -> String -> Module
        getModule ms name = head $ filter (\(Module m _) -> (m == name)) ms

topLevelStatementToCode :: Env -> [Stmt] -> Env
topLevelStatementToCode env stmts = let _typesAndFuncs = statementToCode env $ Seq $ typesAndFuncs stmts
                                        main = statementToCode (set code emptyCode _typesAndFuncs) $ Seq $ filter (\x -> not (isType x) && not (isFunc x)) stmts
                                     in if null (main^.code.source)
                                        then _typesAndFuncs
                                        else over (code . source) (++ "\n" ++ makeMain (main^.code.source)) _typesAndFuncs

typesAndFuncs :: [Stmt] -> [Stmt]
typesAndFuncs stmts = filter (\x -> isType x || isFunc x) stmts

makeMain :: String -> String
makeMain xs = "int main() {\n"
              ++ indent "void* _stack = get_stack();\n"
              ++ indent "init();\n" 
              ++ (indent xs) ++ "\n"
              ++ indent "stack_reset(_stack);\n"
              ++ indent "tear_down();\n"
              ++ indent "return 0;\n"
              ++ "}\n"

isType (Type _ _) = True
isType _ = False

isFunc (Func _ _ _ _) = True
isFunc _ = False

isImport (Import _) = True
isImport _ = False
 
emptyEnv :: Env
emptyEnv = Env stdLib [] emptyCode 

emptyCode :: Code
emptyCode = Code "" ""


statementToCode :: Env -> Stmt -> Env
--statementToCode a b = undefined TODO
statementToCode env (Seq stmts) = foldl statementToCode env stmts 
statementToCode env (Import name) = let a = over (code . header) (++ "#include " ++ name ++ ";\n") env
                                     in over (code . source) (++ "#include " ++ name ++ ";\n") a
statementToCode env (If expr consequent alternative) = let consequentBranch = statementToCode (set code emptyCode env) consequent
                                                           alternativeBranch = statementToCode (set code emptyCode env) alternative
                                                           ifCode = printf "if(%s) {\n%s\n} else {\n%s\n}\n" 
                                                                        (expressionToCode env expr)
                                                                        (indent (consequentBranch^.code.source))
                                                                        (indent (alternativeBranch^.code.source))
                                                        in over (code . source) (++ ifCode) env
statementToCode env (For initial cond increment body) = let initializationEnv = statementToCode (set code emptyCode env) initial
                                                            incrementEnv = simpleStatementToCode (set code emptyCode initializationEnv) increment
                                                            bodyEnv = statementToCode (set code emptyCode incrementEnv) body
                                                            forCode = printf "%s\nfor (; %s; %s) {\n%s\n}\n" 
                                                                            (initializationEnv^.code.source) 
                                                                            (expressionToCode initializationEnv cond) 
                                                                            (incrementEnv^.code.source) 
                                                                            (indent (bodyEnv^.code.source))
                                                         in over (code . source) (++ forCode) env
statementToCode env f@(Func name vars ret body) = let deref = if isPrimitive ret then "" else "*"
                                                      returnStatement = (if (ret /= "Void") then "return " ++ deref ++ "_result;\n" else "")
                                                      funcSignature = printf "%s %s(%s)" (returnTypeToCode ret) name (varsToCode env vars)
                                                      funcCode = printf ("%s {\n"
                                                                ++ indent ("void* _stack = get_stack();\n"
                                                                    ++ (if (ret /= "Void") then (typeToCode ret) ++ " _result;\n" else "")
                                                                    ++ "%s\n"
                                                                    ++ "stack_reset(_stack);\n"
                                                                    ++ returnStatement)
                                                                ++ "}\n\n") funcSignature ((statementToCode (set code emptyCode env) body)^.code.source)
                                                      headerCode = funcSignature ++ ";\n"
                                                  in over functions (++ [f]) $ over (code . header) (++ headerCode) $ over (code . source) (++ funcCode) env
statementToCode env (Type name members) = let typeCode = printf "struct %s {\n%s;\n};\n\n" name (indent (L.intercalate ";\n" (map (varToCode env) members)))
                                           in over (code . header) (++ typeCode) env
statementToCode env (Return expr) = over (code . source) (++ "_result = " ++ expressionToCode env expr ++ ";\n") env
statementToCode env Continue = over (code . source) (++ "continue;\n") env
statementToCode env stmt = over (code . source) (++ ";\n") (simpleStatementToCode env stmt) -- Default

-- | For statements that can be part of for instance a for expression
-- TODO consider cleaner solution, maybe make part of grammar
simpleStatementToCode :: Env -> Stmt -> Env
simpleStatementToCode env Nop = env
simpleStatementToCode env (variable := expr) = over (code . source) (++ (expressionToCode env variable) ++ " = (" ++ expressionToCode env expr ++ ")") env
simpleStatementToCode env (StFuncCall funcCall) = over (code . source) (++ expressionToCode env funcCall) env
simpleStatementToCode env (StVd var) = over (code . source) (++ varToCode env var) env 


expressionToCode :: Env -> Expr -> String
expressionToCode env (Var v) = v
expressionToCode env (MemberAccess v m) = "(*" ++ v ++ ")" ++ "->" ++ m 
expressionToCode env (Con l) = literalToCode l 
expressionToCode env (Uno unop expr) = unopToCode unop ++ "(" ++ expressionToCode env expr ++ ")"
expressionToCode env (Duo duop expr1 expr2) = "(" ++ 
                                         expressionToCode env expr1 ++
                                         ")" ++ 
                                         duopToCode duop ++
                                         "(" ++ 
                                         expressionToCode env expr2 ++
                                         ")"
-- TODO need to take address of result if result is not primitive
expressionToCode env (FuncCall name params) = let t = returnTypeOfFunction env name
                                                  prefix = if isPrimitive t then "" else "stack_push("
                                                  suffix = if isPrimitive t then "" else ")"
                                               in printf "%s%s(%s)%s" prefix name (L.intercalate "," (map (expressionToCode env) params)) suffix
expressionToCode env (New t) = "(" ++ typeToCode t ++ ")" ++ "stack_push(alloc(sizeof(struct " ++ t ++ ")))"

returnTypeOfFunction :: Env -> String -> String
returnTypeOfFunction env funcName = extractFuncReturnType $ head $ filter (\f -> funcName == (extractFuncName f)) (env^.functions)
    where
        extractFuncName (Func name _ _ _) = name -- TODO could probably be a prism
        extractFuncReturnType (Func _ _ returnType _) = returnType -- TODO could probably be a prism

literalToCode :: Literal -> String
literalToCode (BoolLiteral b) = map C.toLower $ show b 
literalToCode (IntLiteral i) = show i
literalToCode (DoubleLiteral d) = show d
literalToCode (StringLiteral s) = "makeString(" ++ show s ++ ")" -- TODO memory leak

varsToCode env vs = L.intercalate ", " $ map (varToCode env) vs

varToCode :: Env -> VarDecl -> String
varToCode env (Single name type_) | isPrimitive type_ = (typeToCode type_) ++ " " ++ name 
                                  | otherwise = (typeToCode type_) ++ " " ++ name 
varToCode env (Array name type_) = (typeToCode type_) ++ "* " ++ name ++ "[256]" -- TODO custom length

typeToCode :: String -> String
typeToCode t | isPrimitive t = map C.toLower t
             | otherwise = "struct " ++ t ++ "**"

returnTypeToCode :: String -> String
returnTypeToCode t | isPrimitive t = map C.toLower t
                   | otherwise = "struct " ++ t ++ "*"

isPrimitive :: String -> Bool
isPrimitive t = t `elem` ["Void", "Int", "Double", "Char", "Bool"] 

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

