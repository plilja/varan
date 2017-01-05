module Util where

indent :: String -> String
indent s = "\t" ++ f s
    where
        f [] = []
        f ['\n'] = ['\n']
        f ('\n':ss) = ['\n', '\t'] ++ f ss
        f (c:ss) = c:f ss

