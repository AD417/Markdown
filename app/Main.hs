{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Markdown.Tokens
import Markdown.Parsing
import Markdown.Assembly

completeParses :: String -> [String]
completeParses x = map (assemble. fst) $ filter (null . snd) $ (parseExpression . tokenize) x


-- Determine the first complete and valid parse on the input string, and return
-- the AST it produces. 
compile :: String -> String
compile source =
    let
        results = completeParses source
    in
    if not $ null results then
        head results
    else
        "Compilation Error"

compileAllPossibilities :: String -> String
compileAllPossibilities source = 
    unlines $ completeParses source

countCompilations :: String -> Int
countCompilations source = length $ completeParses source

main :: IO ()
main = do
    fileText <- readFile "testcat.txt"
    -- putStr $ unlines ( map show $ tokenize fileText )
    -- putStr $ compileAllPossibilities $ "***************"
    print $ countCompilations "*****************"
-- output: "<em>italic and <em>*bold</em> but maybe not?</em>*"
