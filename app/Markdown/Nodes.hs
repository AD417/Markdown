module Markdown.Nodes ( 
    Node(..)
) where

-- Nodes in an AST Tree
data Node
    = Italic Node
    | Underline Node
    | Strikethrough Node
    | Bold Node
    | Concat Node Node
    | Text String
    deriving (Show, Eq)