module SKI (SKI (..), parseSKI) where

data SKI = S | K | I | App [SKI] deriving (Eq)

instance Show SKI where
  show S = "S"
  show K = "K"
  show I = "I"
  show (App xs) = concatMap show xs

parseChar :: Char -> SKI
parseChar 'S' = S
parseChar 'K' = K
parseChar 'I' = I
parseChar _ = App []

parseSKI :: String -> SKI
parseSKI xs = App (map parseChar xs)
