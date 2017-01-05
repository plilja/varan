module StdLib where

import Grammar

stdLib :: [Stmt]
stdLib = [(Type "String" [])
        , (Func "pprint" [(Single "arg" "String")] "void" Nop)
        , (Func "intToString" [(Single "arg" "Int")] "String" Nop)
        , (Func "makeString" [(Array "s" "Char")] "void" Nop)
        ]

