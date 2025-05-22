module Markdown.Parsing (
    parseExpression, Parser
) where

import Markdown.Tokens
import Markdown.Nodes
-- import Debug.Trace (trace)

-- Applicative parser
type Parser t = [Token] -> [(t, [Token])]

-- Read the next symbol as literal text. 
-- This is usually a word.
readText :: Parser Node
readText (Literal c : ts) = [(Text c, ts)]
readText (Star : ts) = [(Text "*", ts)]
readText (Underscore : ts) = [(Text "_", ts)]
readText (Tilde : ts) = [(Text "~", ts)]
readText _ = []

-- read a star, or fail.
readStar :: Parser Token
readStar (Star : ts) = [(Star, ts)]
readStar _ = []

-- read an underscore, or fail.
readUnderscore :: Parser Token
readUnderscore (Underscore : ts) = [(Star, ts)]
readUnderscore _ = []

readTilde :: Parser Token
readTilde (Tilde : ts) = [(Tilde, ts)]
readTilde _ = []


-- apply multiple parsers applicatively.
sequential :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
sequential mapper p1 p2 tokens =
    [(mapper r1 r2, output) | (r1, intermediate) <- p1 tokens
                         , (r2, output) <- p2 intermediate]

-- I blame Yusuf for this. No clue how this works. 
apply :: Parser (b -> c) -> Parser b -> Parser c
apply = sequential ($)

-- A parser that always succeeds, and returns the given "success" value. 
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
    `apply` readStar
    `apply` readStar
    `apply` parseExpression -- e
    `apply` readStar
    `apply` readStar
    -- let boldMark = sequential (\a b -> b) parseStar parseStar
    -- in sequential (\a b -> Bold b) boldMark (sequential const parseExpression boldMark)

-- Parse *italic* text. 
parseItalic :: Parser Node
parseItalic = success (\_ e _ -> Italic e)
    `apply` readStar
    `apply` parseExpression -- e
    `apply` readStar
--parseItalic = sequential (\a b -> Italic b) parseStar (sequential const parseExpression parseStar)

parseUnderline :: Parser Node
parseUnderline = success (\_ _ e _ _ -> Underline e)
    `apply` readUnderscore
    `apply` readUnderscore
    `apply` parseExpression -- e
    `apply` readUnderscore
    `apply` readUnderscore

parseStrikethrough :: Parser Node
parseStrikethrough = success (\_ _ e _ _ -> Strikethrough e)
    `apply` readTilde
    `apply` readTilde
    `apply` parseExpression -- e
    `apply` readTilde
    `apply` readTilde --(trace ('\n' : show x) x)


-- Parse "something" -- perform an operation that, once done, is guaranteed to 
-- progress the input. 
parseSomething :: Parser Node
parseSomething = anyOf [parseBold, parseUnderline, parseStrikethrough, parseItalic, readText]

-- Parse anything that we are able to. 
parseExpression :: Parser Node
parseExpression = oneOf parseSomething parseConcat
