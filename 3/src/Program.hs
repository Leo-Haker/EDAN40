module Program(T, parse, fromString, toString, exec) where

import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
import qualified CoreParser as Statement
import qualified Control.Applicative as Dictionary

newtype T = Program ([Statement], Dictionary)-- to be defined

instance Eq T where
  p1 == p2 = show p1 == show p2
  
instance Show T where
  show = toString

instance Parse T where
  parse = many Statement.parse >-> \stmts -> Program (stmts, Dictionary.empty)
  toString Program (stmts, _)= unlines map Statement.toString stmts

exec :: T -> [Integer] -> [Integer]
exec = Statement.exec
