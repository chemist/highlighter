module Text.Highlighter.Lexers.Velocity (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Velocity"
    , lAliases = ["velocity"]
    , lExtensions = [".vm", ".fhtml"]
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

variable' :: TokenMatcher
variable' =
    [ tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tokNext "\\(" (Arbitrary "Punctuation") (GoTo funcparams')
    , tokNext "(\\.)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Variable")]) Push
    , tokNext "\\}" (Arbitrary "Punctuation") Pop
    , tokNext "" (Arbitrary "Other") Pop
    ]

rangeoperator' :: TokenMatcher
rangeoperator' =
    [ tok "\\.\\." (Arbitrary "Operator")
    , anyOf funcparams'
    , tokNext "\\]" (Arbitrary "Operator") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "[^{#$]+" (Arbitrary "Other")
    , tok "(#)(\\*.*?\\*)(#)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Comment"), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tok "(##)(.*?$)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Comment")])
    , tokNext "(#\\{?)([a-zA-Z_][a-zA-Z0-9_]*)(\\}?)(\\s?\\()" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Punctuation")]) (GoTo directiveparams')
    , tok "(#\\{?)([a-zA-Z_][a-zA-Z0-9_]*)(\\}|\\b)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tokNext "\\$\\{?" (Arbitrary "Punctuation") (GoTo variable')
    ]

funcparams' :: TokenMatcher
funcparams' =
    [ tokNext "\\$\\{?" (Arbitrary "Punctuation") (GoTo variable')
    , tok "\\s+" (Arbitrary "Text")
    , tok "," (Arbitrary "Punctuation")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "0[xX][0-9a-fA-F]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\b[0-9]+\\b" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "(true|false|null)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tokNext "\\(" (Arbitrary "Punctuation") Push
    , tokNext "\\)" (Arbitrary "Punctuation") Pop
    ]

directiveparams' :: TokenMatcher
directiveparams' =
    [ tok "(&&|\\|\\||==?|!=?|[-<>+*%&\\|\\^/])|\\b(eq|ne|gt|lt|ge|le|not|in)\\b" (Arbitrary "Operator")
    , tokNext "\\[" (Arbitrary "Operator") (GoTo rangeoperator')
    , tok "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b" (Arbitrary "Name" :. Arbitrary "Function")
    , anyOf funcparams'
    ]

