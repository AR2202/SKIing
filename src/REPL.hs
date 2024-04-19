module REPL (repl) where

import Eval
import SKI

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
    _ -> print (parse input>>=eval) >> repl'