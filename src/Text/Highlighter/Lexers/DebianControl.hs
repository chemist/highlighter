module Text.Highlighter.Lexers.DebianControl (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Debian Control file"
    , lAliases = ["control"]
    , lExtensions = ["control"]
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

maintainer' :: TokenMatcher
maintainer' =
    [ tok "<[^>]+>" (Arbitrary "Generic" :. Arbitrary "Strong")
    , tokNext "<[^>]+>$" (Arbitrary "Generic" :. Arbitrary "Strong") Pop
    , tok ",\\n?" (Arbitrary "Text")
    , tok "." (Arbitrary "Text")
    ]

depends' :: TokenMatcher
depends' =
    [ tok ":\\s*" (Arbitrary "Text")
    , tok "(\\$)(\\{)(\\w+\\s*:\\s*\\w+)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Entity")])
    , tokNext "\\(" (Arbitrary "Text") (GoTo depend_vers')
    , tok "," (Arbitrary "Text")
    , tok "\\|" (Arbitrary "Operator")
    , tok "[\\s]+" (Arbitrary "Text")
    , tokNext "[}\\)]\\s*$" (Arbitrary "Text") Pop
    , tok "[}]" (Arbitrary "Text")
    , tokNext "[^,]$" (Arbitrary "Name" :. Arbitrary "Function") Pop
    , tok "([\\+\\.a-zA-Z0-9-][\\s\\n]*)" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "\\[.*?\\]" (Arbitrary "Name" :. Arbitrary "Entity")
    ]

root' :: TokenMatcher
root' =
    [ tokNext "^(Description)" (Arbitrary "Keyword") (GoTo description')
    , tokNext "^(Maintainer)(:\\s*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo maintainer')
    , tokNext "^((Build-)?Depends)" (Arbitrary "Keyword") (GoTo depends')
    , tok "^((?:Python-)?Version)(:\\s*)([^\\s]+)$" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number")])
    , tok "^((?:Installed-)?Size)(:\\s*)([^\\s]+)$" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number")])
    , tok "^(MD5Sum|SHA1|SHA256)(:\\s*)([^\\s]+)$" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number")])
    , tok "^([a-zA-Z\\-0-9\\.]*?)(:\\s*)(.*?)$" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Literal" :. Arbitrary "String")])
    ]

description' :: TokenMatcher
description' =
    [ tok "(.*)(Homepage)(: )([^\\s]+)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Name"), (Arbitrary "Name" :. Arbitrary "Class")])
    , tok ":.*\\n" (Arbitrary "Generic" :. Arbitrary "Strong")
    , tok " .*\\n" (Arbitrary "Text")
    , tokNext "" (Arbitrary "Text") Pop
    ]

depend_vers' :: TokenMatcher
depend_vers' =
    [ tokNext "\\)," (Arbitrary "Text") Pop
    , tokNext "\\)[^,]" (Arbitrary "Text") (PopNum 2)
    , tok "([><=]+)(\\s*)([^\\)]+)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number")])
    ]

