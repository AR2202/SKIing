module REPL (repl) where

import Eval
import Parser

repl :: IO ()
repl = do
  putStrLn "Wecome to SKI!"

  putStrLn "please enter ':q'' to quit"

  putStrLn "or enter a valid SKI expression"
  repl'

repl' :: IO ()
repl' = do
  putStrLn "SKI$"
  input <- getLine
  case input of
    ":q" -> putStrLn "goodbye!"
    _ -> case parse input >>= eval of
      Left err -> print err >> repl'
      Right ski -> print ski >> repl'
