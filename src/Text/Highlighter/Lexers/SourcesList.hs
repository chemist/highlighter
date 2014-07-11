module Text.Highlighter.Lexers.SourcesList (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Debian Sourcelist"
    , lAliases = ["sourceslist", "sources.list"]
    , lExtensions = ["sources.list"]
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

distribution' :: TokenMatcher
distribution' =
    [ tokNext "#.*?$" (Arbitrary "Comment") Pop
    , tok "\\$\\(ARCH\\)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[^\\s$[]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\\[" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo escapedDistribution')
    , tok "\\$" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\\s+" (Arbitrary "Text") (GoTo components')
    ]

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "#.*?$" (Arbitrary "Comment")
    , tokNext "^(deb(?:-src)?)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo distribution')
    ]

escapedDistribution' :: TokenMatcher
escapedDistribution' =
    [ tokNext "\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") Pop
    , tok "\\$\\(ARCH\\)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[^\\]$]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tok "\\$" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    ]

components' :: TokenMatcher
components' =
    [ tokNext "#.*?$" (Arbitrary "Comment") (PopNum 2)
    , tokNext "$" (Arbitrary "Text") (PopNum 2)
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\S+" (Arbitrary "Keyword" :. Arbitrary "Pseudo")
    ]

