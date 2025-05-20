module Markdown.Parsing (
    parseExpression, Parser
) where

import Markdown.Tokens
import Markdown.Nodes

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
apply :: Parser (b -> c) -> Parser b -> Parser c
apply = sequential ($)

-- A parser that always succeeds, and returns the "success" value. 
success :: a -> Parser a
success a ts = [(a, ts)]

-- A parser that always fails. 
failed :: Parser a
failed _ = []

-- Determine anything that passes one of(not both of) the parsers. 
oneOf :: Parser a -> Parser a -> Parser a
oneOf p1 p2 x = p1 x ++ p2 x

-- Determine anything that passes any of (not multiple of) the parsers.
anyOf :: [Parser a] -> Parser a
anyOf = foldr oneOf failed

-- Concatenate two expressions. 
parseConcat :: Parser Node
parseConcat = sequential Concat parseSomething parseExpression

-- Parse **bold** text. 
parseBold :: Parser Node
parseBold = success (\_ _ e _ _ -> Bold e) 
    `apply` parseStar 
    `apply` parseStar 
    `apply` parseExpression -- e
    `apply` parseStar 
    `apply` parseStar
    -- let boldMark = sequential (\a b -> b) parseStar parseStar
    -- in sequential (\a b -> Bold b) boldMark (sequential const parseExpression boldMark)

-- Parse *italic* text. 
parseItalic :: Parser Node
parseItalic = success (\_ e _ -> Italic e)
    `apply` parseStar
    `apply` parseExpression -- e
    `apply` parseStar
--parseItalic = sequential (\a b -> Italic b) parseStar (sequential const parseExpression parseStar)

-- Parse "something" -- perform an operation that, once done, is guaranteed to 
-- progress the input. 
parseSomething :: Parser Node 
parseSomething = anyOf [parseBold, parseItalic, parseText]

-- Parse anything that we are able to. 
parseExpression :: Parser Node
parseExpression = oneOf parseConcat parseSomething
