module Markdown.Tokens (
    Token(Literal, Underscore, Star),
    tokenize
) where
-- Tokenization
data Token
    = Literal String
    | Underscore
    | Star
    deriving (Show, Eq)

-- Convert a string into a series of tokens. 
tokenize :: String -> [Token]
tokenize [] = []
tokenize ('\\' : c : cs) = 
    let (s, remainder) = span (/= '*') cs in 
    Literal (c:s)  : tokenize remainder
tokenize ('*' : cs) = Star : tokenize cs
tokenize ('_' : cs) = Underscore : tokenize cs
tokenize cs = 
    let (s, remainder) = span (/= '*') cs in 
    Literal s : tokenize remainder
