module Text.Highlighter.Lexers.NginxConf (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Nginx configuration file"
    , lAliases = ["nginx"]
    , lExtensions = []
    , lMimetypes = ["text/x-nginx-conf"]
    , lStart = root'
    , lFlags = [multiline]
    }

base' :: TokenMatcher
base' =
    [ tok "#.*\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "on|off" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "\\$[^\\s;#()]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "([a-z0-9.-]+)(:)([0-9]+)" (ByGroups [(Arbitrary "Name"), (Arbitrary "Punctuation"), (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")])
    , tok "[a-z-]+/[a-z-+]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "[0-9]+[km]?\\b" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "(\126)(\\s*)([^\\s{]+)" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")])
    , tok "[:=\126]" (Arbitrary "Punctuation")
    , tok "[^\\s;#{}$]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "/[^\\s;#]*" (Arbitrary "Name")
    , tok "\\s+" (Arbitrary "Text")
    , tok "[$;]" (Arbitrary "Text")
    ]

root' :: TokenMatcher
root' =
    [ tok "(include)(\\s+)([^\\s;]+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name")])
    , tokNext "[^\\s;#]+" (Arbitrary "Keyword") (GoTo stmt')
    , anyOf base'
    ]

stmt' :: TokenMatcher
stmt' =
    [ tokNext "{" (Arbitrary "Punctuation") (GoTo block')
    , tokNext ";" (Arbitrary "Punctuation") Pop
    , anyOf base'
    ]

block' :: TokenMatcher
block' =
    [ tokNext "}" (Arbitrary "Punctuation") (PopNum 2)
    , tokNext "[^\\s;#]+" (Arbitrary "Keyword" :. Arbitrary "Namespace") (GoTo stmt')
    , anyOf base'
    ]

