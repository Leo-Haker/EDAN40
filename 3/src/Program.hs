{-# LANGUAGE InstanceSigs #-}
module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary

newtype T = Program [Statement.T]-- to be defined

instance Eq T where
  p1 == p2 = show p1 == show p2
  
instance Show T where
  show = toString

instance Parse T where
  parse = Statement.many Statement.parse >-> \stmts -> Program stmts
  toString = error "Program.toString not implemented"

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.execute stmts Dictionary.empty input
