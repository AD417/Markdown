-- Tokenization
data Token
    = Literal Char
    | Star
    deriving (Show, Eq)

-- Convert a string into a series of tokens.
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : s)
    | c == '*' = Star : tokenize s
    | otherwise = Literal c : tokenize s


-- Parse tokens into an AST
data Node
    = Italic Node
    | Bold Node
    | Concat Node Node
    | Element Char
    | EncodingElement Char
    deriving (Show, Eq)

-- The type of a parser. 
-- It needs to be in this box called `P` because of weird monad syntax shenanigans.
newtype Parser t = Parser ([Token] -> [(t, [Token])])

-- Simulate a parser running. Used by Yusuf's Negative lookahead. 
runParser :: Parser a -> [Token] -> [(a,[Token])]
runParser (Parser p) = p

-- Remember the state of the parse so we can pull it out later.
-- Used by Yusuf's negative lookahead. 
remember :: Parser [Token]
remember = Parser (\ts -> [(ts, ts)])

-- Restore a parser's state. Used by the negative lookahead. 
restore :: [Token] -> Parser ()
restore ts = Parser (const [((), ts)])

-- A parser that is guaranteed to fail. 
failed :: Parser a
failed = Parser (const [])

instance Functor Parser where
    fmap f p = do
        a <- p
        return (f a)


instance Applicative Parser where
    pure a = Parser (\ts -> [(a, ts)])
    p1 <*> p2 = do
        f <- p1
        a <- p2
        return (f a)

instance Monad Parser where
    (Parser p) >>= f = Parser (\ts -> [(b, r) | (a,q) <- p ts
                                              , let (Parser p2) = f a
                                              , (b,r) <- p2 q])
    return = pure

negLookahead :: Parser a -> Parser ()
negLookahead p = do
    ts <- remember
    let results = runParser p ts
    if null results then
        return ()
    else
        failed