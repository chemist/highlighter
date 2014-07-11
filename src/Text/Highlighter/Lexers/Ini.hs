module Text.Highlighter.Lexers.Ini (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "INI"
    , lAliases = ["ini", "cfg"]
    , lExtensions = [".ini", ".cfg"]
    , lMimetypes = ["text/x-ini"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "[;#].*?$" (Arbitrary "Comment")
    , tok "\\[.*?\\]$" (Arbitrary "Keyword")
    , tok "(.*?)([ \\t]*)(=)([ \\t]*)(.*(?:\\n[ \\t].+)*)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Attribute"), (Arbitrary "Text"), (Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")])
    ]

