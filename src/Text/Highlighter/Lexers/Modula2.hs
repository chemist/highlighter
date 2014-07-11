module Text.Highlighter.Lexers.Modula2 (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Modula-2"
    , lAliases = ["modula2", "m2"]
    , lExtensions = [".def", ".mod"]
    , lMimetypes = ["text/x-modula2"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

punctuation' :: TokenMatcher
punctuation' =
    [ tok "[\\(\\)\\[\\]{},.:;|]" (Arbitrary "Punctuation")
    ]

pragmas' :: TokenMatcher
pragmas' =
    [ tok "\\(\\*\\$(.*?)\\*\\)" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "<\\*(.*?)\\*>" (Arbitrary "Comment" :. Arbitrary "Preproc")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\n+" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    ]

numliterals' :: TokenMatcher
numliterals' =
    [ tok "[01]+B" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Binary")
    , tok "[0-7]+B" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "[0-7]+C" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "[0-9A-F]+C" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9A-F]+H" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9]+\\.[0-9]+E[+-][0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9]+\\.[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    ]

operators' :: TokenMatcher
operators' =
    [ tok "[*/+=#\126&<>\\^-]" (Arbitrary "Operator")
    , tok ":=" (Arbitrary "Operator")
    , tok "@" (Arbitrary "Operator")
    , tok "\\.\\." (Arbitrary "Operator")
    , tok "`" (Arbitrary "Operator")
    , tok "::" (Arbitrary "Operator")
    ]

identifiers' :: TokenMatcher
identifiers' =
    [ tok "([a-zA-Z_\\$][a-zA-Z0-9_\\$]*)" (Arbitrary "Name")
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , anyOf comments'
    , anyOf pragmas'
    , anyOf identifiers'
    , anyOf numliterals'
    , anyOf strings'
    , anyOf operators'
    , anyOf punctuation'
    ]

strings' :: TokenMatcher
strings' =
    [ tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    ]

comments' :: TokenMatcher
comments' =
    [ tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*(.*?)\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "\\(\\*([^\\$].*?)\\*\\)" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

