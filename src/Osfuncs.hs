module OsFuncs where

import Grammar

stdLib :: [Stmt]
stdLib = [(Func "pprint" [(Single "arg" "String")] "Void" Nop)
        , (Func "printInt" [(Single "arg" "Int")] "Void" Nop)
        , (Func "pprintInt" [(Single "arg" "Int")] "Void" Nop) 
        , (Func "print" [(Single "arg" "String")] "Void" Nop)
        , (Func "assert" [(Single "arg" "Bool")] "Void" Nop)
        ]

