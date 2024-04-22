{-# LANGUAGE DeriveGeneric #-}

module SKI (SKI (..), SKIError (..), SKIToken (..)) where

import GHC.Generics

-- import Generic.Random (genericArbitrary', uniform)
data SKI = S | K | I | App [SKI] deriving (Eq, Generic)

data SKIToken = SToken | IToken | KToken | ParensOpen | ParensClose deriving (Show, Read, Eq, Generic)

data SKIError = ParserError | SyntaxError | EvalError deriving (Show, Read, Eq)

instance Show SKI where
  show S = "S"
  show K = "K"
  show I = "I"
  show (App xs) = concatMap show xs
