module Text.Highlighter.Lexers.Redcode (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Redcode"
    , lAliases = ["redcode"]
    , lExtensions = [".cw"]
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok ";.*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "\\b(DAT|MOV|ADD|SUB|MUL|DIV|MOD|JMP|JMZ|JMN|DJN|CMP|SLT|SPL|ORG|EQU|END)\\b" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "\\b(A|B|AB|BA|F|X|I)\\b" (Arbitrary "Name" :. Arbitrary "Decorator")
    , tok "[A-Za-z_][A-Za-z_0-9]+" (Arbitrary "Name")
    , tok "[-+*/%]" (Arbitrary "Operator")
    , tok "[#$@<>]" (Arbitrary "Operator")
    , tok "[.,]" (Arbitrary "Punctuation")
    , tok "[-+]?\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    ]

