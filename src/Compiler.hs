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
    let libFolder = head args
        codeFiles = (libFolder ++ "/varan_std.vr"):(tail args)
    modules <- mapM parseCodeFromFile codeFiles
    let compiledModules = modulesToCode modules
    mapM_ (\((Module name _), code) -> writeCodeFile name code) compiledModules

writeCodeFile :: String -> Code -> IO ()
writeCodeFile moduleName (Code header source) = do
    let helper = (\f content -> do
                        oFile <- openFile ("target/" ++ f) WriteMode
                        hPutStrLn oFile $ "#ifndef " ++ (replace f '.' '_')
                        hPutStrLn oFile $ "#define " ++ (replace f '.' '_')
                        hPutStrLn oFile "#include \"osfuncs.h\""
                        hPutStrLn oFile "#include \"memory.h\""
                        hPutStrLn oFile ""
                        hPutStrLn oFile content
                        hPutStrLn oFile $ "#endif"
                        hClose oFile
                 )
    helper (moduleName ++ ".h") header
    helper (moduleName ++ ".c") ("#include \"" ++ moduleName ++ ".h\"\n" ++ source)

getOutFile :: String -> String -> String
getOutFile f contents | "int main() {" `L.isInfixOf` contents = f ++ ".c" -- TODO solve with real header files instead
                      | otherwise = f ++ ".h"

dropSuffix ('.':xs) | '.' `elem` xs = '.':dropSuffix xs
                    | otherwise = ""
dropSuffix (x:xs) = x:dropSuffix xs
dropFolders xs | '/' `elem` xs = dropFolders (tail xs)
               | otherwise = xs

parseCodeFromFile :: String -> IO Module
parseCodeFromFile fp = do 
    let res = parseFromFile program fp
        moduleName = dropFolders $ dropSuffix fp
    either (\err -> error (show err)) (\c -> Module moduleName c) <$> res
