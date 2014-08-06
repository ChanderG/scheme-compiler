module Core where

import Test.HUnit
import Compiler

test1 :: Test
test1 = TestCase (assertEqual
                   "Simple string"
									 (readExpr "Haskell")
									 "Found value")

test2 :: Test
test2 = TestCase (assertEqual
                   "Simple atom"
									 (readExpr "_Haskell")
									 "Found value")

test3 :: Test
test3 = TestCase (assertEqual
                   "Simple number"
									 (readExpr "1234")
									 "Found value")

tests = TestList [test1,test2,test3] 
