import Data.Bifunctor (first)
type Parser t = String -> [(t, String)]

type Node = Bool -- Until I figure out what this should be...

parseLeft :: Parser Node
parseLeft ('(' : s) = [(True, s)]
parseLeft _ = []

parseRight :: Parser Node
parseRight (')' : s) = [(True, s)]
parseRight _ = []

parseChar :: Parser Node
parseChar ('(' : s) = []
parseChar (')' : s) = []
parseChar (c : s) = [(True, s)]
parseChar _ = []

oneOf :: Parser a -> Parser b -> Parser (Either a b)
oneOf p1 p2 s =
       map (first Left) (p1 s)
    ++ map (first Right) (p2 s)
    -- let out1 = [(a, r) | (a, r) <- p1 s]
    --     out2 = [(b, r) | (b, r) <- p2 s]
    -- in 
    -- if not (null out1) then out1
    -- else if not (null out2) then out2
    -- else []

sequential :: Parser a -> Parser b -> Parser (a,b)
sequential p1 p2 s = [((a,b), r) | (a, q) <- p1 s
                                 , (b, r) <- p2 s]

success :: Parser ()
success s = [((), s)]

kleeneStar :: Parser a -> Parser [a]
kleeneStar p1 s = ([],s) : concat [map (first (x:)) $ kleeneStar p1 r | (x,r) <- p1 s]

    --map (first (:[])) p1 s 
    --oneOf (sequential p1 $ kleeneStar p1) success


parseWord :: Parser [Node]
parseWord = kleeneStar parseChar

parseParen :: Parser a
parseParen = let (lP, (e, rP)) = sequential (sequential parseLeft parseExpr) parseRight
             in e

parseExpr :: Parser a
parseExpr = oneOf parseParen parseWord
