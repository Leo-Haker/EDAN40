{- Testfor Statement -}
module TestStatement (tests) where

import Test.HUnit

import Statement 

import qualified Dictionary

-- Tests parsing
-- Tests that parsing and printing return the original
-- You might want to add your own!

testParse0 :: Test
testParse0 = 
    let s = "skip;"
        parsed = fromString s :: Statement.T
    in s ~?= toString parsed

-- These tests are important cause they show how to use the code.
testParse1 :: Test
testParse1 = 
    let str = "read count;"
        stmt = fromString str :: Statement.T
    in str ~?= toString stmt

testParse2 :: Test
testParse2 = 
    let str = "write count+1;"
        stmt = fromString str :: Statement.T
    in str ~?= toString stmt

testsParsing :: Test
testsParsing = TestList [ testParse0, testParse1, testParse2 ]

-- Tests execytion

p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11 :: Statement.T 

testp1, testp2, testp3, testp4, testp5, testp6, testp7, testp8, testp9, testp10, testp11 :: Test

p1 = fromString "skip;"
testp1 = execute [p1] env stdin ~?= []
    where env = Dictionary.empty
          stdin = []

p2 = fromString "read count;"
testp2 = execute [p2] env stdin ~?= []
    where env = Dictionary.empty
          stdin = []

p3 = fromString "write count+1;"
testp3 = execute [p3] env stdin ~?= [3] -- Should write 3
    where env = Dictionary.insert ("count", 2) Dictionary.empty -- The env maps 'count' to the value 2
          stdin = []

p4 = fromString "count := 0; write count;"
testp4 = execute [p4] env stdin ~?= [0] -- assignment set variable to 0
    where env = Dictionary.empty 
          stdin = []

p5 = fromString "begin skip; end"
testp5 = execute [p5] env stdin ~?= []
    where env = Dictionary.empty
          stdin = []

p6 = fromString "begin x:=0; x:=x+1; write x; end"
testp6 = execute [p6] env stdin ~?= [1]
    where env = Dictionary.empty
          stdin = []

p7 = fromString "if x then skip; else x:=0-x; write x;"
testp7 = execute [p7] env stdin ~?= [-1]
    where env = Dictionary.insert ("x", 1) Dictionary.empty
          stdin = [] 

-- This should NOT get stuck in an infinite loop. 
-- Should loop until n is under zero 
p8 = fromString "while n do n:=n-1;"
testp8 = execute [p8] env stdin ~?= []
    where env = Dictionary.insert ("n", 3) Dictionary.empty
          stdin = []

s9 :: String
s9 = "while n do begin fac:=fac*n; n:=n-1; end"
p9 = fromString s9
testp9 = execute [p9] env stdin ~?= []
    where env = Dictionary.insert ("n", 3) Dictionary.empty
          stdin = []

p10 = fromString  "begin read x ; x := x + 1 ; write x; end"
testp10 = execute [p10] env stdin ~?= [4]
    where env = Dictionary.empty
          stdin = [3]

p11 = fromString  ("begin read n; fac:=1; " ++ s9 ++ " write fac; end")
testp11 = execute [p11] env stdin ~?= [24]
    where env = Dictionary.empty
          stdin = [4]

testsExecution :: Test
testsExecution = TestList [testp1, testp2, testp3,
    testp4, testp5, testp6,
    testp7, testp8, testp9,
    testp10, testp11]

tests :: Test
tests = TestList [testsParsing, testsExecution]


