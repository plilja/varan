module Main
    where

import Parser
import Grammar
import StdLib
import CodeGenerator
import Control.Arrow
import System.Environment
import Text.Parsec as P
import Text.Parsec.String

main = do
    program <- getArgs >>= parseCodeFromFile
    let std = stdLib
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

