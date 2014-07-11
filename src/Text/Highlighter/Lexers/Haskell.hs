module Text.Highlighter.Lexers.Haskell (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Haskell"
    , lAliases = ["haskell", "hs"]
    , lExtensions = [".hs"]
    , lMimetypes = ["text/x-haskell"]
    , lStart = root'
    , lFlags = [multiline]
    }

comment' :: TokenMatcher
comment' =
    [ tok "[^-{}]+" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "{-" (Arbitrary "Comment" :. Arbitrary "Multiline") Push
    , tokNext "-}" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tok "[-{}]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

funclist' :: TokenMatcher
funclist' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "[A-Z][a-zA-Z0-9_]*" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "[_a-z][\\w\\']*" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "--.*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "{-" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo comment')
    , tok "," (Arbitrary "Punctuation")
    , tok "[:!#$%&*+.\\\\/<=>?@^|\126-]+" (Arbitrary "Operator")
    , tokNext "\\(" (Arbitrary "Punctuation") (DoAll [(GoTo funclist'), (GoTo funclist')])
    , tokNext "\\)" (Arbitrary "Punctuation") (PopNum 2)
    ]

string' :: TokenMatcher
string' =
    [ tok "[^\\\\\"]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\\\\" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") (GoTo escape')
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

import' :: TokenMatcher
import' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tokNext "\\)" (Arbitrary "Punctuation") Pop
    , tok "qualified(?![\\w'])" (Arbitrary "Keyword")
    , tokNext "([A-Z][a-zA-Z0-9_.]*)(\\s+)(as)(\\s+)([A-Z][a-zA-Z0-9_.]*)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Namespace"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name")]) Pop
    , tokNext "([A-Z][a-zA-Z0-9_.]*)(\\s+)(hiding)(\\s+)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Namespace"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Punctuation")]) (GoTo funclist')
    , tokNext "([A-Z][a-zA-Z0-9_.]*)(\\s+)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Namespace"), (Arbitrary "Text"), (Arbitrary "Punctuation")]) (GoTo funclist')
    , tokNext "[a-zA-Z0-9_.]+" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

escape' :: TokenMatcher
escape' =
    [ tokNext "[abfnrtv\"\\'&\\\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") Pop
    , tokNext "\\^[][A-Z@\\^_]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") Pop
    , tokNext "NUL|SOH|[SE]TX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|S[OI]|DLE|DC[1-4]|NAK|SYN|ETB|CAN|EM|SUB|ESC|[FGRU]S|SP|DEL" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") Pop
    , tokNext "o[0-7]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") Pop
    , tokNext "x[\\da-fA-F]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") Pop
    , tokNext "\\d+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") Pop
    , tokNext "\\s+\\\\" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "--(?![!#$%&*+./<=>?@\\^|_\126]).*?$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "{-" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo comment')
    , tokNext "(?<![\\w'])import(?![\\w'])" (Arbitrary "Keyword" :. Arbitrary "Reserved") (GoTo import')
    , tokNext "(?<![\\w'])module(?![\\w'])" (Arbitrary "Keyword" :. Arbitrary "Reserved") (GoTo module')
    , tok "(?<![\\w'])error(?![\\w'])" (Arbitrary "Name" :. Arbitrary "Exception")
    , tok "(?<![\\w'])(case|class|data|default|deriving|do|else|if|in|infix[lr]?|instance|let|newtype|of|then|type|where|_)(?!\\')(?![\\w'])" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "^[_a-z][\\w\\']*" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "[_a-z][\\w\\']*" (Arbitrary "Name")
    , tok "[A-Z][\\w\\']*" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "\\\\(?![:!#$%&*+.\\\\/<=>?@^|\126-]+)" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "(<-|::|->|=>|=)(?![:!#$%&*+.\\\\/<=>?@^|\126-]+)" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok ":[:!#$%&*+.\\\\/<=>?@^|\126-]*" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "[:!#$%&*+.\\\\/<=>?@^|\126-]+" (Arbitrary "Operator")
    , tok "\\d+[eE][+-]?\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+\\.\\d+([eE][+-]?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[oO][0-7]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[xX][\\da-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tokNext "'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char") (GoTo character')
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "\\[\\]" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "\\(\\)" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[][(),;`{}]" (Arbitrary "Punctuation")
    ]

module' :: TokenMatcher
module' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "([A-Z][a-zA-Z0-9_.]*)(\\s+)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Namespace"), (Arbitrary "Text"), (Arbitrary "Punctuation")]) (GoTo funclist')
    , tokNext "[A-Z][a-zA-Z0-9_.]*" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

character' :: TokenMatcher
character' =
    [ tok "[^\\\\']" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tokNext "\\\\" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") (GoTo escape')
    , tokNext "'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char") Pop
    ]

