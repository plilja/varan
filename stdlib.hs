module StdLib where

import Grammar

stdLib :: [Stmt]
stdLib = [(Type "String" [])
        , (Func "print" [(Single "arg" "String")] "void" Nop)
        , (Func "intToString" [(Single "arg" "Int")] "String" Nop)
        ]

