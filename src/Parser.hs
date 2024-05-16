module Parser (parse, tokenize) where

import SKI

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
    parseParens exp (ParensClose : xs) = parseSKI exp : parsed xs
    parseParens _ [] = [Left ParserError]
    parseParens exp (x : xs) = parseParens ( exp ++[x]) xs

tokenize :: [Char] -> Either SKIError [SKIToken]
tokenize = traverse parseChar

parse :: [Char] -> Either SKIError SKI
parse s = tokenize s >>= parseSKI
