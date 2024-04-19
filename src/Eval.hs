module Eval
  ( eval,
  )
where

import SKI

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
    Right exp -> case eval (App (y : [z])) of
      Left err -> Left err
      Right exp2 -> eval $ App (exp : exp2 : xs)
eval (App (App xs : ys)) = eval (App (xs ++ ys))
eval _ = Left EvalError
