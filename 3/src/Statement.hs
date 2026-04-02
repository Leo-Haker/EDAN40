module Statement(T, parse, toString, fromString, execute) where

import Parser hiding (T)
import Prelude hiding (return, fail)
import qualified Dictionary
import qualified Expr

class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

type T = Statement
data Statement =
    Skip | 
    Read String | 
    Write Expr.T | 
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Unknown -- Replace this with legit stuff
    deriving Show

-- PARSING

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip :: Parser Statement
skip = accept "skip" -# return Skip

read :: Parser Statement
read = accept "read" -# word >-> Read

write :: Parser Statement
write = accept "write" -# Expr.parse >-> Write

statement = skip #- require ";" ! 
    Statement.read #- require ";" !
    Statement.write #- require ";"

-- EXECUTION

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Skip:stmts) dict input = exec stmts dict input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    let res = (Expr.value cond dict)
    in case res of 
        Left err -> error err
        Right x -> 
            if x > 0 
            then exec (thenStmts: stmts) dict input
            else exec (elseStmts: stmts) dict input

instance Executable Statement where
    execute = exec

instance Parse Statement where
  parse = statement
  toString Skip = "skip;"
  toString (Read v) = "read " <> v <> ";"
  toString (Write e) = "write " <> toString e <> ";"
  toString _ = error "Statement.toString not implemented"
