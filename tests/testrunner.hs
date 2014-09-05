module Main where

import Test.HUnit
import Core as C

main :: IO Counts 
main = runTestTT C.tests 

