module Main where
import System.Environment 

--statements of do need to be aligned
main = do {-
          args <- getArgs
          argst <- getArgs  --any number of getArgs gets the same args passed in 
          putStrLn("Hello " ++ args !! 0)
          putStrLn("Hello " ++ argst !! 1)
          -}
          
          --empty lines in between work 
          putStrLn "Enter your name: "
          name <- getLine
          putStrLn("Hello, " ++ name) 
 
          {-
          args <- getArgs
          putStrLn("Bye " ++  show(read(args !! 0) + 10))
          -} 
          
