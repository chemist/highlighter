module Text.Highlighter.Lexers.Gas (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "GAS"
    , lAliases = ["gas"]
    , lExtensions = [".s", ".S"]
    , lMimetypes = ["text/x-gas"]
    , lStart = root'
    , lFlags = [multiline]
    }

directiveArgs' :: TokenMatcher
directiveArgs' =
    [ tok "(?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+)" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "\"(\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "@(?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+)" (Arbitrary "Name" :. Arbitrary "Attribute")
    , tok "(?:0[xX][a-zA-Z0-9]+|\\d+)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tokNext "[\\r\\n]+" (Arbitrary "Text") Pop
    , tokNext "#.*?$" (Arbitrary "Comment") Pop
    , anyOf punctuation'
    , anyOf whitespace'
    ]

punctuation' :: TokenMatcher
punctuation' =
    [ tok "[-*,.():]+" (Arbitrary "Punctuation")
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tok "(?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+):" (Arbitrary "Name" :. Arbitrary "Label")
    , tokNext "\\.(?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+)" (Arbitrary "Name" :. Arbitrary "Attribute") (GoTo directiveArgs')
    , tok "lock|rep(n?z)?|data\\d+" (Arbitrary "Name" :. Arbitrary "Attribute")
    , tokNext "(?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+)" (Arbitrary "Name" :. Arbitrary "Function") (GoTo instructionArgs')
    , tok "[\\r\\n]+" (Arbitrary "Text")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "#.*?\\n" (Arbitrary "Comment")
    ]

instructionArgs' :: TokenMatcher
instructionArgs' =
    [ tok "([a-z0-9]+)( )(<)((?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+))(>)" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Constant"), (Arbitrary "Punctuation")])
    , tok "([a-z0-9]+)( )(<)((?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+))([-+])((?:0[xX][a-zA-Z0-9]+|\\d+))(>)" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Constant"), (Arbitrary "Punctuation"), (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer"), (Arbitrary "Punctuation")])
    , tok "(?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+)" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "(?:0[xX][a-zA-Z0-9]+|\\d+)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "%(?:[a-zA-Z$_][a-zA-Z$._0-9@]*|\\.[a-zA-Z$._0-9@]+)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "$(?:0[xX][a-zA-Z0-9]+|\\d+)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tokNext "[\\r\\n]+" (Arbitrary "Text") Pop
    , tokNext "#.*?$" (Arbitrary "Comment") Pop
    , anyOf punctuation'
    , anyOf whitespace'
    ]

