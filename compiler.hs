module Main
    where

import Parser
import Grammar
import OsFuncs
import CodeGenerator
import Control.Arrow
import System.Environment
import Text.Parsec as P
import Text.Parsec.String

main = do
    stdlib <- parseCodeFromFile ["stdlib.vr"]
    putStrLn (statementToCode stdlib)
    program <- getArgs >>= parseCodeFromFile
    let osFuncs = stdLib
        code = statementToCode program
    putStrLn code
    putStrLn ""
    putStrLn "//---------"
    putStrLn ""
    putStrLn $ "// " ++ (show program)

parseCodeFromFile :: [String] -> IO Stmt
parseCodeFromFile fp = do 
    let res = parseFromFile program (fp !! 0) -- TODO multiple input files
    either (\err -> error (show err)) id <$> res

