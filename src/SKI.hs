module SKI (SKI (..)) where

data SKI = S | K | I | App [SKI] deriving (Eq)

instance Show SKI where
  show S = "S"
  show K = "K"
  show I = "I"
  show (App xs) = concatMap show xs
