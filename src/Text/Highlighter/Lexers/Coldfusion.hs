module Text.Highlighter.Lexers.Coldfusion (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "cfstatement"
    , lAliases = ["cfs"]
    , lExtensions = []
    , lMimetypes = []
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "//.*" (Arbitrary "Comment")
    , tok "\\+\\+|--" (Arbitrary "Operator")
    , tok "[-+*/^&=!]" (Arbitrary "Operator")
    , tok "<=|>=|<|>" (Arbitrary "Operator")
    , tok "mod\\b" (Arbitrary "Operator")
    , tok "(eq|lt|gt|lte|gte|not|is|and|or)\\b" (Arbitrary "Operator")
    , tok "\\|\\||&&" (Arbitrary "Operator")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo string')
    , tok "'.*?'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "(if|else|len|var|case|default|break|switch)\\b" (Arbitrary "Keyword")
    , tok "([A-Za-z_$][A-Za-z0-9_.]*)\\s*(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Punctuation")])
    , tok "[A-Za-z_$][A-Za-z0-9_.]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[()\\[\\]{};:,.\\\\]" (Arbitrary "Punctuation")
    , tok "\\s+" (Arbitrary "Text")
    ]

string' :: TokenMatcher
string' =
    [ tok "\"\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "#.+?#" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interp")
    , tok "[^\"#]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "#" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") Pop
    ]

