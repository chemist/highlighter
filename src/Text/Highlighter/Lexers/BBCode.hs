module Text.Highlighter.Lexers.BBCode (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "BBCode"
    , lAliases = ["bbcode"]
    , lExtensions = []
    , lMimetypes = ["text/x-bbcode"]
    , lStart = root'
    , lFlags = [multiline]
    }

tag' :: TokenMatcher
tag' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "(\\w+)(=)(\"?[^\\s\"\\]]+\"?)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Attribute"), (Arbitrary "Operator"), (Arbitrary "Literal" :. Arbitrary "String")])
    , tok "(=)(\"?[^\\s\"\\]]+\"?)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Literal" :. Arbitrary "String")])
    , tokNext "\\]" (Arbitrary "Keyword") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "[^[]+" (Arbitrary "Text")
    , tokNext "\\[/?\\w+" (Arbitrary "Keyword") (GoTo tag')
    , tok "\\[" (Arbitrary "Text")
    ]

