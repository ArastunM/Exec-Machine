module Main where

import System.Environment
import Text.Read
import Compiler
import Interpreter

--TODO Task 3.4
main :: IO ()
main = do
  args <- getArgs

  let readCommand = readMaybe (head args) :: Maybe Com
  case readCommand of
    Nothing -> putStrLn "Invalid Command, Try Again"
    Just x -> print(ccomp x)
