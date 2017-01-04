module StdLib where

import Grammar

stdLib :: [Stmt]
stdLib = [(Type "String" [])
        , (Func "print" [(Vd "arg" "String")] "void" Nop)
        , (Func "intToString" [(Vd "arg" "Int")] "String" Nop)
        ]

