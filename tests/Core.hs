module Core where

import Test.HUnit
import Compiler



-- tests readExpr with ^
test1 :: Test
test1 = TestCase (assertEqual
                   "To check if a simple symbol is parsed correctly"
									 (readExpr "^")
									 "Found value")

-- tests readExpr with ^
test2 :: Test
test2 = TestCase (assertEqual
                   "Symbol parser with space"
									 (readExpr " ^")
									 "Found value")


tests = TestList [test1, test2] 
