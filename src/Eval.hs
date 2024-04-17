module Eval
  ( eval,
  )
where

import SKI

eval I = I
eval K = K
eval S = S
eval (App []) = App []
eval (App [x]) = eval x
eval (App (I : xs)) = eval (App xs)
eval (App (K : [x])) = App (K : [x])
eval (App (K : x : y : xs)) = eval (App (x : xs))
eval (App (S : [x])) = App (S : [x])
eval (App (S : x : [y])) = App (S : x :[y])
eval (App (S : x : y : z : xs)) = eval (App (eval (App (x : [y])) : eval (App (x : [z])) : xs))
