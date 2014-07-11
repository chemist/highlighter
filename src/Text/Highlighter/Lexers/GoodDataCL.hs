module Text.Highlighter.Lexers.GoodDataCL (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "GoodData-CL"
    , lAliases = ["gooddata-cl"]
    , lExtensions = [".gdc"]
    , lMimetypes = ["text/x-gooddata-cl"]
    , lStart = root'
    , lFlags = [caseless]
    }

argsList' :: TokenMatcher
argsList' =
    [ tokNext "\\)" (Arbitrary "Punctuation") Pop
    , tok "," (Arbitrary "Punctuation")
    , tok "[a-zA-Z]\\w*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "=" (Arbitrary "Operator")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo stringLiteral')
    , tok "[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]{1,3})?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\s" (Arbitrary "Text")
    ]

root' :: TokenMatcher
root' =
    [ tok "#.*" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "[a-zA-Z]\\w*" (Arbitrary "Name" :. Arbitrary "Function")
    , tokNext "\\(" (Arbitrary "Punctuation") (GoTo argsList')
    , tok ";" (Arbitrary "Punctuation")
    , tok "\\s+" (Arbitrary "Text")
    ]

stringLiteral' :: TokenMatcher
stringLiteral' =
    [ tok "\\\\[tnrfbae\"\\\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "[^\\\\\"]+" (Arbitrary "Literal" :. Arbitrary "String")
    ]

