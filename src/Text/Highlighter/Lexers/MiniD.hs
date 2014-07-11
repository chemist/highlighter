module Text.Highlighter.Lexers.MiniD (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "MiniD"
    , lAliases = ["minid"]
    , lExtensions = [".md"]
    , lMimetypes = ["text/x-minidsrc"]
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
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "/\\+" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo nestedcomment')
    , tok "(as|assert|break|case|catch|class|continue|coroutine|default|do|else|finally|for|foreach|function|global|namespace|if|import|in|is|local|module|return|super|switch|this|throw|try|vararg|while|with|yield)\\b" (Arbitrary "Keyword")
    , tok "(false|true|null)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "([0-9][0-9_]*)?\\.[0-9_]+([eE][+\\-]?[0-9_]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[Bb][01_]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0[Cc][0-7_]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[xX][0-9a-fA-F_]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "(0|[1-9][0-9_]*)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "'(\\\\['\"?\\\\abfnrtv]|\\\\x[0-9a-fA-F]{2}|\\\\[0-9]{1,3}|\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8}|.)'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "@\"(\"\"|.)*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "`(``|.)*`" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "(\126=|\\^=|%=|\\*=|==|!=|>>>=|>>>|>>=|>>|>=|<=>|\\?=|-\\>|<<=|<<|<=|\\+\\+|\\+=|--|-=|\\|\\||\\|=|&&|&=|\\.\\.|/=)|[-/.&$@|\\+<>!()\\[\\]{}?,;:=*%^\126#\\\\]" (Arbitrary "Punctuation")
    , tok "[a-zA-Z_]\\w*" (Arbitrary "Name")
    ]

