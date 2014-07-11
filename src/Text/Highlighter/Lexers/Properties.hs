module Text.Highlighter.Lexers.Properties (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Properties"
    , lAliases = ["properties"]
    , lExtensions = [".properties"]
    , lMimetypes = ["text/x-java-properties"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "(?:[;#]|//).*$" (Arbitrary "Comment")
    , tok "(.*?)([ \\t]*)([=:])([ \\t]*)(.*(?:(?<=\\\\)\\n.*)*)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Attribute"), (Arbitrary "Text"), (Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")])
    ]

