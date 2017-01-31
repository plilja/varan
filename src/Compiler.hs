module Main
    where

import Parser
import Grammar
import OsFuncs
import Util
import CodeGenerator
import Control.Arrow
import System.Environment
import Text.Parsec as P
import Text.Parsec.String
import System.IO
import qualified Data.List as L
import qualified Data.Maybe as M

main = do
    args <- getArgs
    mapM_ compileSingleFile args
    compileSingleFile "lib/varan_std.vr"

compileSingleFile :: String -> IO ()
compileSingleFile fileName = do
    program <- parseCodeFromFile fileName
    let outFileName = getOutFile fileName
        (code, maybeMain) = programToCode program
    writeCodeFile outFileName code []
    case maybeMain of
        Just m -> writeCodeFile "main.c" m [outFileName]
        _ -> return ()

writeCodeFile :: String -> String -> [String] -> IO ()
writeCodeFile outFileName contents includes = do
    oFile <- openFile ("target/" ++ outFileName) WriteMode
    hPutStrLn oFile $ "#ifndef " ++ (replace outFileName '.' '_')
    hPutStrLn oFile $ "#define " ++ (replace outFileName '.' '_')
    hPutStrLn oFile "#include \"osfuncs.h\""
    hPutStrLn oFile "#include \"memory.h\""
    hPutStrLn oFile $ unlines $ map (\s -> "#include \"" ++ s ++ "\"") includes
    if outFileName /= "varan_std.h" then
        hPutStrLn oFile "#include \"varan_std.h\""
    else
        return ()

    hPutStrLn oFile ""
    hPutStrLn oFile contents
    hPutStrLn oFile $ "#endif"
    hClose oFile

getOutFile :: String -> String
getOutFile f = dropFolders (dropSuffix f) ++ ".h"
    where
        dropSuffix ('.':xs) | '.' `elem` xs = '.':dropSuffix xs
                            | otherwise = ""
        dropSuffix (x:xs) = x:dropSuffix xs
        dropFolders xs | '/' `elem` xs = dropFolders (tail xs)
                       | otherwise = xs

parseCodeFromFile :: String -> IO Stmt
parseCodeFromFile fp = do 
    let res = parseFromFile program fp
    either (\err -> error (show err)) id <$> res
