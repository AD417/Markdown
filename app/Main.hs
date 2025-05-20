{-# OPTIONS_GHC -Wno-unused-top-binds #-}
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

compileAllPossibilities :: String -> String
compileAllPossibilities source = 
    let
        allParses = (parseExpression . tokenize) source
        completeParses = filter (null . snd) allParses
    in
    unlines ( map (assemble . fst) completeParses)

main :: IO ()
main = putStr $ compileAllPossibilities "_*italic and **bold* but maybe not?**_"
-- output: "<em>italic and <em>*bold</em> but maybe not?</em>*"
