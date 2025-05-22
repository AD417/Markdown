module Markdown.Tokens (
    Token(..),
    tokenize
) where

-- Tokenization
data Token
    = Literal String
    | Underscore
    | Tilde
    | Star
    deriving (Show, Eq)

tokenConversions :: [(Char, Token)]
tokenConversions = [
    ('*', Star),
    ('_', Underscore),
    ('~', Tilde)]

usedSymbols :: [Char]
usedSymbols = map fst tokenConversions

-- Convert a string into a series of tokens. 
tokenize :: String -> [Token]
tokenize "" = []
tokenize ('\\' : c : cs) =
    let (s, remainder) = span (`notElem` usedSymbols) cs in
    Literal (c:s)  : tokenize remainder
tokenize (c : cs) =
    let match = filter (\x -> c == fst x) tokenConversions in
    if null match then
        let (s, remainder) = span (`notElem` usedSymbols) cs in
        Literal (c:s)  : tokenize remainder
    else
        let (_, tokenType) = head match in
        tokenType : tokenize cs
