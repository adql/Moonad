module Main (main) where

import Control.Exception (SomeException(..), handle)
import System.Environment (getArgs, getProgName)

import Eval
import Parser

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if length args /= 1
    then putStrLn $ "Usage: " ++ name ++ " program.cow"
    else evalFile $ head args
  return ()

evalFile :: String -> IO ()
evalFile file = handle (\(SomeException _)
                         -> putStrLn $ "Can't open file " ++ file)
  $ readFile file >>= runCow . parseCow >> return ()
