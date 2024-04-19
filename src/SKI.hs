module SKI (SKI (..), SKIError(..),parseSKI, parse) where

data SKI = S | K | I | App [SKI] deriving (Eq)

data SKIToken = SToken | IToken | KToken | ParensOpen | ParensClose deriving (Show, Read, Eq)

data SKIError = ParserError | SyntaxError | EvalError deriving (Show, Read, Eq)

instance Show SKI where
  show S = "S"
  show K = "K"
  show I = "I"
  show (App xs) = concatMap show xs

parseChar :: Char -> Either SKIError SKIToken
parseChar 'S' = Right SToken
parseChar 'K' = Right KToken
parseChar 'I' = Right IToken
parseChar '(' = Right ParensOpen
parseChar ')' = Right ParensClose
parseChar _ = Left SyntaxError

parseSKI :: [SKIToken] -> Either SKIError SKI
parseSKI xs = case sequenceA $ parsed xs of
  Left err -> Left err
  Right skis -> Right $ App skis
  where
    parsed [] = []
    parsed (SToken : xs) = Right S : parsed xs
    parsed (KToken : xs) = Right K : parsed xs
    parsed (IToken : xs) = Right I : parsed xs
    parsed (ParensOpen : xs) = parseParens [] xs
    parseParens exp (ParensClose : xs) = (App <$> sequenceA exp) : parsed xs
    parseParens _ [] = [Left ParserError]

    parseParens exp (x : xs) = parseParens ((parsed[ x]) ++ exp) xs

tokenize :: [Char] -> Either SKIError [SKIToken]
tokenize = traverse parseChar

parse s = tokenize s >>= parseSKI
