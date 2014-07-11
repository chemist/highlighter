module Text.Highlighter.Lexers.Ragel (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Ragel"
    , lAliases = ["ragel"]
    , lExtensions = []
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

host' :: TokenMatcher
host' =
    [ tok "([^{}\\'\"/#]+|[^\\\\][\\\\][{}]|\"(\\\\\\\\|\\\\\"|[^\"])*\"|'(\\\\\\\\|\\\\'|[^'])*'|//.*$\\n?|/\\*(.|\\n)*?\\*/|\\#.*$\\n?|/(?!\\*)(\\\\\\\\|\\\\/|[^/])*/|/)+" (Arbitrary "Other")
    , tokNext "{" (Arbitrary "Punctuation") Push
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

literals' :: TokenMatcher
literals' =
    [ tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\[(\\\\\\\\|\\\\\\]|[^\\]])*\\]" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "/(?!\\*)(\\\\\\\\|\\\\/|[^/])*/" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\s+" (Arbitrary "Text" :. Arbitrary "Whitespace")
    ]

keywords' :: TokenMatcher
keywords' =
    [ tok "(access|action|alphtype)\\b" (Arbitrary "Keyword")
    , tok "(getkey|write|machine|include)\\b" (Arbitrary "Keyword")
    , tok "(any|ascii|extend|alpha|digit|alnum|lower|upper)\\b" (Arbitrary "Keyword")
    , tok "(xdigit|cntrl|graph|print|punct|space|zlen|empty)\\b" (Arbitrary "Keyword")
    ]

operators' :: TokenMatcher
operators' =
    [ tok "," (Arbitrary "Operator")
    , tok "\\||&|-|--" (Arbitrary "Operator")
    , tok "\\.|<:|:>|:>>" (Arbitrary "Operator")
    , tok ":" (Arbitrary "Operator")
    , tok "->" (Arbitrary "Operator")
    , tok "(>|\\$|%|<|@|<>)(/|eof\\b)" (Arbitrary "Operator")
    , tok "(>|\\$|%|<|@|<>)(!|err\\b)" (Arbitrary "Operator")
    , tok "(>|\\$|%|<|@|<>)(\\^|lerr\\b)" (Arbitrary "Operator")
    , tok "(>|\\$|%|<|@|<>)(\126|to\\b)" (Arbitrary "Operator")
    , tok "(>|\\$|%|<|@|<>)(\\*|from\\b)" (Arbitrary "Operator")
    , tok ">|@|\\$|%" (Arbitrary "Operator")
    , tok "\\*|\\?|\\+|{[0-9]*,[0-9]*}" (Arbitrary "Operator")
    , tok "!|\\^" (Arbitrary "Operator")
    , tok "\\(|\\)" (Arbitrary "Operator")
    ]

identifiers' :: TokenMatcher
identifiers' =
    [ tok "[a-zA-Z_][a-zA-Z_0-9]*" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

root' :: TokenMatcher
root' =
    [ anyOf literals'
    , anyOf whitespace'
    , anyOf comments'
    , anyOf keywords'
    , anyOf numbers'
    , anyOf identifiers'
    , anyOf operators'
    , tokNext "{" (Arbitrary "Punctuation") (GoTo host')
    , tok "=" (Arbitrary "Operator")
    , tok ";" (Arbitrary "Punctuation")
    ]

comments' :: TokenMatcher
comments' =
    [ tok "\\#.*$" (Arbitrary "Comment")
    ]

numbers' :: TokenMatcher
numbers' =
    [ tok "0x[0-9A-Fa-f]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[+-]?[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    ]

