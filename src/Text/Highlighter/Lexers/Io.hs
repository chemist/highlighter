module Text.Highlighter.Lexers.Io (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Io"
    , lAliases = ["io"]
    , lExtensions = [".io"]
    , lMimetypes = ["text/x-iosrc"]
    , lStart = root'
    , lFlags = [multiline]
    }

nestedcomment' :: TokenMatcher
nestedcomment' =
    [ tok "[^+/]+" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "/\\+" (Arbitrary "Comment" :. Arbitrary "Multiline") Push
    , tokNext "\\+/" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tok "[+/]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

root' :: TokenMatcher
root' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "//(.*?)\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "#(.*?)\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "/\\+" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo nestedcomment')
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "::=|:=|=|\\(|\\)|;|,|\\*|-|\\+|>|<|@|!|/|\\||\\^|\\.|%|&|\\[|\\]|\\{|\\}" (Arbitrary "Operator")
    , tok "(clone|do|doFile|doString|method|for|if|else|elseif|then)\\b" (Arbitrary "Keyword")
    , tok "(nil|false|true)\\b" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "(Object|list|List|Map|args|Sequence|Coroutine|File)\8" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    , tok "(\\d+\\.?\\d*|\\d*\\.\\d+)([eE][+-]?[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    ]

