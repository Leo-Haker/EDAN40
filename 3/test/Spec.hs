import Test.HUnit

import qualified TestParser
import qualified TestExpr
import qualified TestStatement
import qualified TestProgram

main :: IO ()
main = runTestTTAndExit $ TestList [TestParser.tests, TestExpr.tests, TestStatement.tests, TestProgram.tests]
