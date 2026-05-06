module Statement(T, parse, toString, fromString, execute) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import Data.Tuple (uncurry)
import CoreParser (Parse(parse))
import qualified CoreParser as Expr

type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Begin [Statement] |
    While Expr.T Statement|
    Read String |
    Write Expr.T|
    Skip String |
    Comment String
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment
-- eftersom vi får (cond, (then, else)) o vill ha If cond then else
if' :: Parser Statement
if' = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> \((cond, thenB), elseB) -> If cond thenB elseB

begin :: Parser Statement
begin = accept "begin" -# many parse #- require "end" >-> Begin

while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> uncurry While

read' :: Parser Statement
read' = accept "read" -# word #- require ";" >-> Read

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> Write

skip :: Parser Statement 
skip = accept "skip" #- require ";" >-> Skip

comment :: Parser Statement
comment = Parser.comment >-> Comment

many :: Parser a -> Parser [a]
many p = some p ! return []

some :: Parser a -> Parser [a]
some p = p #> \x -> many p #> \xs -> return (x:xs)

class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where
    -- execute :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]
    execute [] _ _ = []
    execute (Assignment str expr: stmts) dict input = 
        case Expr.value expr dict of
            Left s -> error s
            Right v -> 
                execute stmts (Dictionary.insert (str, v) dict) input
            -- slänger in värdet på str platsen o sen kör vi vidare

    execute (If cond thenStmts elseStmts: stmts) dict input =
        case Expr.value cond dict of
            Left s -> error s
            Right v ->
                if v > 0 then
                    execute (thenStmts: stmts) dict input
                else
                    execute (elseStmts: stmts) dict input

    -- stmt är in lista så vi kör ++
    execute (Begin stmt:stmts) dict input = execute (stmt ++ stmts) dict input

    execute (While cond thenstmt:stmts) dict input = 
        case Expr.value cond dict of
            Left s -> error s
            Right v -> 
                if v > 0 then
                    -- här kör vi först then blocket sen kör vi hela while igen + resten
                    execute (thenstmt : While cond thenstmt : stmts) dict input
                else
                    execute stmts dict input
            
    execute (Read str : stmts) dict input =
        case input of 
            -- eller kasta ett error med no input ex
            [] -> execute stmts dict input
            x:xs -> 
                execute stmts (Dictionary.insert (str, x) dict) xs
    execute (Write expr : stmts) dict input = 
        case Expr.value expr dict of
            Left s -> error s
            Right v ->
                v : execute stmts dict input
    execute (Skip _: stmts) dict input = execute stmts dict input
    execute (Comment _: stmts) dict input = execute stmts dict input

instance Parse Statement where
-- mucho importante att assignment är till höger då keywords spelar roll i ordningen pga require/accept
  parse = begin ! if' ! while ! read' ! write ! skip ! Statement.comment ! assignment 
  toString = error "Statement.toString not implemented"
