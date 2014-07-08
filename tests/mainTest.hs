module Main where
import Test.HUnit

hi :: Int
hi = 4
he :: Int
he = 5 

test1 :: Test
--test1 = TestCase (assertEqual "Just an Hello World test" 1 1)
test1 = TestCase (assertEqual
                   "Hello Test" 
                   hi
                   he)

test2 :: Test
--test1 = TestCase (assertEqual "Just an Hello World test" 1 1)
test2 = TestCase (assertEqual
                   "Hello Test" 
                   hi
                   he)
--tests :: Test
tests = TestList [test1, test2] 
--tests = TestList [] 


--main :: IO Counts 
main = runTestTT tests

