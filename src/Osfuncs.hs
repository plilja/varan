module OsFuncs where

import Grammar

stdLib :: [Stmt]
stdLib = [(Func "pprint" [(Single "arg" "String")] "Void" Nop)
        , (Func "pprintInt" [(Single "arg" "Int")] "Void" Nop) 
        , (Func "assert" [(Single "arg" "Bool")] "Void" Nop)
        ]

