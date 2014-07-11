module Text.Highlighter.Lexers.Groff (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Groff"
    , lAliases = ["groff", "nroff", "man"]
    , lExtensions = [".[1234567]", ".man"]
    , lMimetypes = ["application/x-troff", "text/troff"]
    , lStart = root'
    , lFlags = [multiline]
    }

request' :: TokenMatcher
request' =
    [ tokNext "\\n" (Arbitrary "Text") Pop
    , anyOf escapes'
    , tok "\"[^\\n\"]+\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\S+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\s+" (Arbitrary "Text")
    ]

escapes' :: TokenMatcher
escapes' =
    [ tok "\\\\\"[^\\n]*" (Arbitrary "Comment")
    , tok "\\\\[fn]\\w" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "\\\\\\(.." (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "\\\\.\\[.*\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "\\\\." (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tokNext "\\\\\\n" (Arbitrary "Text") (GoTo request')
    ]

root' :: TokenMatcher
root' =
    [ tokNext "(?i)(\\.)(\\w+)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Keyword")]) (GoTo request')
    , tokNext "\\." (Arbitrary "Punctuation") (GoTo request')
    , tokNext "[^\\\\\\n]*" (Arbitrary "Text") (GoTo textline')
    ]

textline' :: TokenMatcher
textline' =
    [ anyOf escapes'
    , tok "[^\\\\\\n]+" (Arbitrary "Text")
    , tokNext "\\n" (Arbitrary "Text") Pop
    ]

