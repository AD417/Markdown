module Markdown.Nodes ( 
    Node(Italic, Underline, Bold, Concat, Text)
) where

-- Nodes in an AST Tree
data Node
    = Italic Node
    | Underline Node
    | Bold Node
    | Concat Node Node
    | Text String
    deriving (Show, Eq)