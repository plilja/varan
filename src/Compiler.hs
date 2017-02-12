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
    mapM_ (\(Module name _, code) -> writeCodeFile name code) compiledModules

writeCodeFile :: String -> String -> IO ()
writeCodeFile moduleName contents = do
    let outFileName = getOutFile moduleName contents
    oFile <- openFile ("target/" ++ outFileName) WriteMode
    hPutStrLn oFile $ "#ifndef " ++ (replace outFileName '.' '_')
    hPutStrLn oFile $ "#define " ++ (replace outFileName '.' '_')
    hPutStrLn oFile "#include \"osfuncs.h\""
    hPutStrLn oFile "#include \"memory.h\""
    if outFileName /= "varan_std.h" then
        hPutStrLn oFile "#include \"varan_std.h\""
    else
        return ()
    hPutStrLn oFile ""
    hPutStrLn oFile contents
    hPutStrLn oFile $ "#endif"
    hClose oFile

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
