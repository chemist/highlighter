module Text.Highlighter.Lexers.Go (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Go"
    , lAliases = ["go"]
    , lExtensions = [".go"]
    , lMimetypes = ["text/x-gosrc"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "//(.*?)\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "(break|default|func|interface|select|case|defer|go|map|struct|chan|else|goto|package|switch|const|fallthrough|if|range|type|continue|for|import|return|var)\\b" (Arbitrary "Keyword")
    , tok "(uint8|uint16|uint32|uint64|int8|int16|int32|int64|float32|float64|byte|uint|int|float|uintptr|string|close|closed|len|cap|new|make)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "\\d+(\\.\\d+[eE][+\\-]?\\d+|\\.\\d*|[eE][+\\-]?\\d+)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\.\\d+([eE][+\\-]?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[0-7]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[xX][0-9a-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "(0|[1-9][0-9]*)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "'(\\\\['\"\\\\abfnrtv]|\\\\x[0-9a-fA-F]{2}|\\\\[0-7]{1,3}|\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8}|[^\\\\])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "`[^`]*`" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "(<<=|>>=|<<|>>|<=|>=|&\\^=|&\\^|\\+=|-=|\\*=|/=|%=|&=|\\|=|&&|\\|\\||<-|\\+\\+|--|==|!=|:=|\\.\\.\\.)|[+\\-*/%&|^<>=!()\\[\\]{}.,;:]" (Arbitrary "Punctuation")
    , tok "[a-zA-Z_]\\w*" (Arbitrary "Name")
    ]

