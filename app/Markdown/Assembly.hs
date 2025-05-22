module Markdown.Assembly (assemble) where

import Markdown.Nodes

-- Convert the AST recursively into a string.
assemble :: Node -> String
assemble (Text c) = c
assemble (Concat x y) = assemble x ++ assemble y
assemble (Bold n) = "<b>" ++ assemble n ++ "</b>"
assemble (Italic n) = "<em>" ++ assemble n ++ "</em>"
assemble (Underline n) = "<u>" ++ assemble n ++ "</u>"
assemble (Strikethrough n) = "<s>" ++ assemble n ++ "</s>"
