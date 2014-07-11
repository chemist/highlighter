module Text.Highlighter.Lexers.Tex (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "TeX"
    , lAliases = ["tex", "latex"]
    , lExtensions = [".tex", ".aux", ".toc"]
    , lMimetypes = ["text/x-tex", "text/x-latex"]
    , lStart = root'
    , lFlags = [multiline]
    }

displaymath' :: TokenMatcher
displaymath' =
    [ tokNext "\\\\\\]" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tokNext "\\$\\$" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\$" (Arbitrary "Name" :. Arbitrary "Builtin")
    , anyOf math'
    ]

general' :: TokenMatcher
general' =
    [ tok "%.*?\\n" (Arbitrary "Comment")
    , tok "[{}]" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[&_^]" (Arbitrary "Name" :. Arbitrary "Builtin")
    ]

command' :: TokenMatcher
command' =
    [ tok "\\[.*?\\]" (Arbitrary "Name" :. Arbitrary "Attribute")
    , tok "\\*" (Arbitrary "Keyword")
    , tokNext "" (Arbitrary "Text") Pop
    ]

inlinemath' :: TokenMatcher
inlinemath' =
    [ tokNext "\\\\\\)" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tokNext "\\$" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf math'
    ]

root' :: TokenMatcher
root' =
    [ tokNext "\\\\\\[" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick") (GoTo displaymath')
    , tokNext "\\\\\\(" (Arbitrary "Literal" :. Arbitrary "String") (GoTo inlinemath')
    , tokNext "\\$\\$" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick") (GoTo displaymath')
    , tokNext "\\$" (Arbitrary "Literal" :. Arbitrary "String") (GoTo inlinemath')
    , tokNext "\\\\([a-zA-Z]+|.)" (Arbitrary "Keyword") (GoTo command')
    , anyOf general'
    , tok "[^\\\\$%&_^{}]+" (Arbitrary "Text")
    ]

math' :: TokenMatcher
math' =
    [ tok "\\\\([a-zA-Z]+|.)" (Arbitrary "Name" :. Arbitrary "Variable")
    , anyOf general'
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[-=!+*/()\\[\\]]" (Arbitrary "Operator")
    , tok "[^=!+*/()\\[\\]\\\\$%&_^{}0-9-]+" (Arbitrary "Name" :. Arbitrary "Builtin")
    ]

