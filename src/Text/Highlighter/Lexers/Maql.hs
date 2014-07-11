module Text.Highlighter.Lexers.Maql (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "MAQL"
    , lAliases = ["maql"]
    , lExtensions = [".maql"]
    , lMimetypes = ["text/x-gooddata-maql", "application/x-gooddata-maql"]
    , lStart = root'
    , lFlags = [caseless]
    }

root' :: TokenMatcher
root' =
    [ tok "IDENTIFIER\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "\\{[^}]+\\}" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]{1,3})?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo stringLiteral')
    , tok "\\<\\>|\\!\\=" (Arbitrary "Operator")
    , tok "\\=|\\>\\=|\\>|\\<\\=|\\<" (Arbitrary "Operator")
    , tok "\\:\\=" (Arbitrary "Operator")
    , tok "\\[[^]]+\\]" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Class")
    , tok "(DIMENSIONS?|BOTTOM|METRIC|COUNT|OTHER|FACT|WITH|TOP|OR|ATTRIBUTE|CREATE|PARENT|FALSE|ROWS?|FROM|ALL|AS|PF|COLUMNS?|DEFINE|REPORT|LIMIT|TABLE|LIKE|AND|BY|BETWEEN|EXCEPT|SELECT|MATCH|WHERE|TRUE|FOR|IN|WITHOUT|FILTER|ALIAS|ORDER|FACT|WHEN|NOT|ON|KEYS|KEY|FULLSET|PRIMARY|LABELS|LABEL|VISUAL|TITLE|DESCRIPTION|FOLDER|ALTER|DROP|ADD|DATASET|DATATYPE|INT|BIGINT|DOUBLE|DATE|VARCHAR|DECIMAL|SYNCHRONIZE|TYPE|DEFAULT|ORDER|ASC|DESC|HYPERLINK|INCLUDE|TEMPLATE|MODIFY)\\b" (Arbitrary "Keyword")
    , tok "[a-zA-Z]\\w*\\b" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "#.*" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "[,;\\(\\)]" (Arbitrary "Punctuation")
    , tok "\\s+" (Arbitrary "Text")
    ]

stringLiteral' :: TokenMatcher
stringLiteral' =
    [ tok "\\\\[tnrfbae\"\\\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "[^\\\\\"]+" (Arbitrary "Literal" :. Arbitrary "String")
    ]

