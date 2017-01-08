module Util where

indent :: String -> String
indent s = "\t" ++ f s
    where
        f [] = []
        f ['\n'] = ['\n']
        f ('\n':ss) = ['\n', '\t'] ++ f ss
        f (c:ss) = c:f ss

replace :: String -> Char -> Char -> String
replace [] _ _ = []
replace (x:xs) a b | x == a = b:replace xs a b
                   | otherwise = x:replace xs a b

