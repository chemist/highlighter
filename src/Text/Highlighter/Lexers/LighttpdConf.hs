module Text.Highlighter.Lexers.LighttpdConf (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Lighttpd configuration file"
    , lAliases = ["lighty", "lighttpd"]
    , lExtensions = []
    , lMimetypes = ["text/x-lighttpd-conf"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "#.*\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\S*" (Arbitrary "Name")
    , tok "[a-zA-Z._-]+" (Arbitrary "Keyword")
    , tok "\\d+\\.\\d+\\.\\d+\\.\\d+(?:/\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "=>|=\126|\\+=|==|=|\\+" (Arbitrary "Operator")
    , tok "\\$[A-Z]+" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[(){}\\[\\],]" (Arbitrary "Punctuation")
    , tok "\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "\\s+" (Arbitrary "Text")
    ]

