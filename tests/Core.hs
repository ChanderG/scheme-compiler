module Core where

import Test.HUnit
--import Main

hi :: Int
hi = 4*2
he :: Int
he = 8 

test1 :: Test
test1 = TestCase (assertEqual
                   "Hello Test" 
                   hi
                   he)


test2 :: Test
test2 = TestCase (assertEqual "Just an Hello World test" 1 1)


tests = TestList [test1, test2] 
