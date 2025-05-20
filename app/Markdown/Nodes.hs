module Markdown.Nodes ( 
    Node(Italic, Bold, Concat, Text)
) where

-- Nodes in an AST Tree
data Node
    = Italic Node
    | Bold Node
    | Concat Node Node
    | Text String
    deriving (Show, Eq)