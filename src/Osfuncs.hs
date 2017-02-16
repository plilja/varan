module OsFuncs where

import Grammar

stdLib :: [Stmt]
stdLib = [(Func "print" [(Single "arg" "String")] "Void" Nop)
        , (Func "printInt" [(Single "arg" "Int")] "Void" Nop) 
        , (Func "assert" [(Single "arg" "Bool")] "Void" Nop)
        ]

