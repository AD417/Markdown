-- Tokenization
data Token
    = Literal String
    | Star
    deriving (Show, Eq)

-- Convert a string into a series of tokens. 
tokenize :: String -> [Token]
tokenize [] = []
tokenize ('\\' : c : cs) = 
    let (s, remainder) = span (/= '*') cs in 
    Literal (c:s)  : tokenize remainder
tokenize ('*' : cs) = Star : tokenize cs
tokenize cs = 
    let (s, remainder) = span (/= '*') cs in 
    Literal s : tokenize remainder

-- Nodes in an AST Tree
data Node
    = Italic Node
    | Bold Node
    | Concat Node Node
    | Text String
    deriving (Show, Eq)

-- Applicative parser
type Parser t = [Token] -> [(t, [Token])]

-- read the next symbol as literal text. 
-- This is usually a word. In some cases it can be a symbol that would
-- otherwise have some meaning. 
parseText :: Parser Node
parseText (Literal c : ts) = [(Text c, ts)]
parseText (Star : ts) = [(Text "*", ts)]
parseText _ = []

-- Parse a star
parseStar :: Parser Token
parseStar (Star : ts) = [(Star, ts)]
parseStar _ = []


-- apply multiple parsers applicatively.
sequential :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
sequential mapper p1 p2 tokens =
    [(mapper r1 r2, output) | (r1, intermediate) <- p1 tokens
                         , (r2, output) <- p2 intermediate]

-- I blame Yusuf for this. No clue how this works. 
apply = sequential ($)

-- A parser that always succeeds, and returns the "success" value. 
success :: a -> Parser a
success a ts = [(a, ts)]

-- A parser that always fails. 
failed :: Parser a
failed ts = []

-- Determine anything that passes one of(not both of) the parsers. 
oneOf :: Parser a -> Parser a -> Parser a
oneOf p1 p2 x = p1 x ++ p2 x

-- Determine anything that passes any of (not multiple of) the parsers.
anyOf :: [Parser a] -> Parser a
anyOf = foldr oneOf failed

-- Concatenate two expressions. 
parseConcat :: Parser Node
parseConcat = sequential Concat parseSomething parseExpr

-- Parse **bold** text. 
parseBold :: Parser Node
parseBold = success (\_ _ e _ _ -> Bold e) 
    `apply` parseStar 
    `apply` parseStar 
    `apply` parseExpr -- e
    `apply` parseStar 
    `apply` parseStar
    -- let boldMark = sequential (\a b -> b) parseStar parseStar
    -- in sequential (\a b -> Bold b) boldMark (sequential const parseExpr boldMark)

-- Parse *italic* text. 
parseItalic :: Parser Node
parseItalic = success (\_ e _ -> Italic e)
    `apply` parseStar
    `apply` parseExpr -- e
    `apply` parseStar
--parseItalic = sequential (\a b -> Italic b) parseStar (sequential const parseExpr parseStar)

-- Parse "something" -- perform an operation that, once done, is guaranteed to 
-- progress the input. 
parseSomething :: Parser Node 
parseSomething = anyOf [parseBold, parseItalic, parseText]

-- Parse anything that we are able to. 
parseExpr :: Parser Node
parseExpr = oneOf parseConcat parseSomething

-- Convert the AST recursively into a string.
assemble :: Node -> String
assemble (Text c) = c
assemble (Concat x y) = assemble x ++ assemble y
assemble (Bold n) = "<b>" ++ assemble n ++ "</b>"
assemble (Italic n) = "<em>" ++ assemble n ++ "</em>"

-- Determine the first complete and valid parse on the input string, and return
-- the AST it produces. 
firstCompleteParse :: String -> Node
firstCompleteParse s =
    let
        allParses = parseExpr $ tokenize s
        (node, out) = head $ filter (\(n, arr) -> null arr) allParses
    in
    if null out
    then node
    else Text "*"

main = print $ assemble $ firstCompleteParse "*italic and **bold* but maybe not?**"

