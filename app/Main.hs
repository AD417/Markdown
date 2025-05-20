module Main (main) where

import Markdown.Tokens
import Markdown.Nodes
import Markdown.Parsing
import Markdown.Assembly

-- Determine the first complete and valid parse on the input string, and return
-- the AST it produces. 
firstCompleteParse :: String -> Node
firstCompleteParse s =
    let
        allParses = parseExpression $ tokenize s
        (node, out) = head $ filter (\(_, arr) -> null arr) allParses
    in
    if null out
    then node
    else Text "*"

main :: IO ()
main = print $ assemble $ firstCompleteParse "*italic and **bold* but maybe not?**"

