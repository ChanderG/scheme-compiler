module Core where

import Test.HUnit
import Compiler

test1 :: Test
test1 = TestCase (assertEqual
                   "Simple string"
                   (testExpr "Haskell")
                   "Found Haskell")

test2 :: Test
test2 = TestCase (assertEqual
                   "Simple atom"
                   (testExpr "_Haskell")
                   "Found _Haskell")

test3 :: Test
test3 = TestCase (assertEqual
                   "Simple number"
                   (testExpr "1234")
                   "Found 1234")

test4 :: Test
test4 = TestCase (assertEqual
                   "Negative case symbol"
                   (testExpr "#")
                   "No match")

test5 :: Test
test5 = TestCase (assertEqual
                   "Expr in paran"
                   (testExpr "(by)")
                   "Found (by)")

test6 :: Test
test6 = TestCase (assertEqual
                   "list of expr"
                   (testExpr "(by hi)")
                   "Found (by hi)")

test7 :: Test
test7 = TestCase (assertEqual
                   "Dotted list of expr"
                   (testExpr "(by . hi)")
                   "Found (by . hi)")

test8 :: Test
test8 = TestCase (assertEqual
                   "nested paran"
                   (testExpr "(hi (hi))")
                   "Found (hi (hi))")

test9 :: Test
test9 = TestCase (assertEqual
                   "unbalanced paran needs to be rejected"
                   (testExpr "(hi (hi)")
                   "No match")

test10 :: Test
test10 = TestCase (assertEqual
                   ". and () nesting "
                   (testExpr "(a (dotted . list) test)")
                   "Found (a (dotted . list) test)")

test11 :: Test
test11 = TestCase (assertEqual
                   "quoted text"
                   (testExpr "(a '(quoted (dotted . list)) test)") 
                   "Found (a (quote (quoted (dotted . list))) test)")

test12 :: Test
test12 = TestCase (assertEqual
                   "test of unwords - list"
                   (testExpr "(1 2 2)") 
                   "Found (1 2 2)")

tests = TestList [test1,test2,test3,test4,
                 test5, test6, test7, test8, test9, test10, test11, test12]
