module Main
    where

import Parser
import Grammar
import CodeGenerator
import Control.Arrow
import System.Environment
import Text.Parsec as P
import Text.Parsec.String

main = do
    program <- getArgs >>= parseCodeFromFile
    let code = statementToCode program
    putStrLn code
    print program

parseCodeFromFile :: [String] -> IO Stmt
parseCodeFromFile fp = do 
    let res = parseFromFile program (fp !! 0) -- TODO multiple input files
    either (\err -> error (show err)) id <$> res

