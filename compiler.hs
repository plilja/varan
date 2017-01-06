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
    compileSingleFile "varan_std.vr"

compileSingleFile :: String -> IO ()
compileSingleFile fileName = do
    program <- parseCodeFromFile fileName
    let outFileName = getOutFile fileName
        code = programToCode program
        maybeMain = mainToCode program
    writeCodeFile outFileName code
    case maybeMain of
        Just m -> writeCodeFile "main.c" m
        _ -> return ()
    putStrLn ""
    putStrLn "//---------"
    putStrLn ""
    putStrLn $ "// " ++ (show program)

writeCodeFile :: String -> String -> IO ()
writeCodeFile outFileName contents = do
    oFile <- openFile ("target/" ++ outFileName) WriteMode
    hPutStrLn oFile $ "#ifndef " ++ (replace outFileName '.' '_')
    hPutStrLn oFile $ "#define " ++ (replace outFileName '.' '_')
    hPutStrLn oFile "#include \"osfuncs.h\""
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
        dropSuffix xs = everythingBefore '.' xs
        dropFolders xs = reverse $ everythingBefore '/' (reverse xs)
        everythingBefore c xs = let rev = reverse xs
                                    i = M.fromMaybe (-1) $ L.elemIndex c rev
                                    rem = drop (i + 1) rev
                                 in reverse rem

parseCodeFromFile :: String -> IO Stmt
parseCodeFromFile fp = do 
    let res = parseFromFile program fp
    either (\err -> error (show err)) id <$> res
