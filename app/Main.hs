module Main (main) where

import Markdown.Tokens
import Markdown.Parsing
import Markdown.Assembly

-- Determine the first complete and valid parse on the input string, and return
-- the AST it produces. 
compile :: String -> String
compile source =
    let
        allParses = (parseExpression . tokenize) source
        completeParses = filter (null . snd) allParses
    in
    if not $ null completeParses then
        (assemble . fst . head) completeParses
    else
        "Compilation Error"

main :: IO ()
main = print $ compile "*italic and **bold* but maybe not?**"
-- output: "<em>italic and <em>*bold</em> but maybe not?</em>*"
