data Token
    = Literal Char
    | OpenParen
    | CloseParen
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : s)
    | c == '(' = OpenParen : tokenize s
    | c == ')' = CloseParen : tokenize s
    | otherwise = Literal c : tokenize s

data Node
    = Expr Node
    | Paren Node
    | Concat Node Node
    | Element Char
    deriving (Show, Eq)


type Parser t = [Token] -> [(t, [Token])]

-- read "("
parseOpen :: Parser Token
parseOpen (OpenParen : ts) = [(OpenParen, ts)]
parseOpen _ = []

-- read ")"
parseClose :: Parser Token
parseClose (CloseParen : ts) = [(CloseParen, ts)]
parseClose _ = []

-- read a character
parseElement :: Parser Node
parseElement (Literal c : ts) = [(Element c, ts)]
parseElement _ = []

-- apply these parsers sequentially
sequential :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
sequential mapper p1 p2 a =
    [(mapper r1 r2, out) | (r1, mid) <- p1 a
                         , (r2, out) <- p2 mid]

-- evaluate all results from both parsers
oneOf :: Parser a -> Parser a -> Parser a
oneOf p1 p2 x = p1 x ++ p2 x


-- success :: Parser ()
-- success s = [((), s)]

--kleeneStar :: Parser a -> Parser 

parseParen :: Parser Node
parseParen = sequential (\a b -> Paren b) parseOpen (sequential const parseExpr parseClose)

parseConcat :: Parser Node
parseConcat = sequential Concat parseElement parseExpr

parseExpr :: Parser Node
parseExpr = oneOf parseParen (oneOf parseElement parseConcat)

main = let (ast, _) = head $ parseExpr $ tokenize "((((((xxxxx))))))" in putStr $ assemble 0 ast 




-- Convert AST into "compiled" code
assemble :: Int -> Node -> String
assemble _ (Element c) = [c]
assemble depth (Concat x y) = assemble depth x ++ assemble depth y
assemble depth (Paren n) = 
    let indent = concat (replicate depth "    ") in 
        "(\n" ++ indent ++ "    " ++ assemble (depth + 1) n ++ "\n" ++ indent ++ ")"
assemble depth (Expr n) = assemble depth n
