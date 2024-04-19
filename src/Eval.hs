module Eval
  ( eval,
  )
where

import SKI

eval :: SKI -> Either SKIError SKI
eval I = Right I
eval K = Right K
eval S = Right S
eval (App []) = Right $ App []
eval (App [x]) = eval x
eval (App (I : xs)) = eval (App xs)
eval (App (K : [x])) = Right $ App (K : [x])
eval (App (K : x : y : xs)) = eval (App (x : xs))
eval (App (S : [x])) = Right $ App (S : [x])
eval (App (S : x : [y])) = Right $ App (S : x : [y])
eval (App (S : x : y : z : xs)) =
  case eval (App (x : [z])) of
    Left err -> Left err
    Right expr -> case eval (App (y : [z])) of
      Left err -> Left err
      Right expr2 -> eval $ App (expr : expr2 : xs)
eval (App (App xs : ys)) = eval (App (xs ++ ys))
--eval _ = Left EvalError
