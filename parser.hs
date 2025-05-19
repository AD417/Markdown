main = let (ast, _) = head $ parseExpr $ tokenize "((((yyy((xxxxx))))))" in putStr $ assemble 0 ast 

-- Tokenization
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


-- Parse tokens into an AST
data Node
    = Expr Node
    | Paren Node
    | Concat Node Node
    | Element Char
    deriving (Show, Eq)

type Parser = [Token] -> [(Node, [Token])]

parseElement :: Parser
parseElement ((Literal c) : ts) = [(Element c, ts)]
parseElement _ = []

parseParen :: Parser
parseParen (OpenParen : ts) = [(Paren x, r) | (x, CloseParen : r) <- parseExpr ts]
parseParen _ = []

parseConcat :: Parser
parseConcat ts = [(Concat x y, r) | (x, q) <- parseElement ts ++ parseParen ts
                                  , (y, r) <- parseExpr q]

parseExpr :: Parser
parseExpr ts = concat [parseElement ts, parseParen ts, parseConcat ts]


-- Convert AST into "compiled" code
assemble :: Int -> Node -> String
assemble _ (Element c) = [c]
assemble depth (Concat x y) = assemble depth x ++ assemble depth y
assemble depth (Paren n) = 
    let indent = concat (replicate depth "    ") in 
        "(\n" ++ indent ++ "    " ++ assemble (depth + 1) n ++ "\n" ++ indent ++ ")"
assemble depth (Expr n) = assemble depth n
