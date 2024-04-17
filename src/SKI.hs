module SKI (SKI (..)) where

data SKI = S | K | I | App [SKI]  deriving (Show, Eq)
