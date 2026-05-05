module Statement(T, parse, toString, fromString, execute) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import Data.Tuple (uncurry)
import CoreParser (Parse(parse))
import Control.Monad.State (State)
import qualified Text.Parsec as Statement
import qualified CoreParser as Expr

type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Begin [Statement] |
    While Expr.T Statement|
    Read String |
    Write Expr.T|
    Skip 
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment
-- eftersom vi får (cond, (then, else)) o vill ha If cond then else
if' = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> uncurry (uncurry . If)

begin = accept "begin" -# many parse #- require "end" >-> Begin

while = accept "while" -# Expr.parse #- require "do" # parse >-> uncurry While

read = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

skip = accept "skip" #- require ";" >-> Skip

many :: Parser a -> Parser [a]
many p = some p ! return []

some :: Parser a -> Parser [a]
some p = p #> \x -> many p #> \xs -> return [x:xs]

class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where
    -- execute :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]
    execute (If cond thenStmts elseStmts: stmts) dict input =
        case (Expr.value cond dict) of
            Left err -> error err
            Right v ->
                if v > 0 then
                    execute (thenStmts: stmts) dict input
                else
                    execute (elseStmts: stmts) dict input

instance Parse Statement where
-- mucho importante att assignment är till höger då keywords spelar roll i ordningen pga require
  parse = begin ! if' ! while ! read ! write ! skip ! assignment 
  toString = error "Statement.toString not implemented"
