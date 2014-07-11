module Text.Highlighter.Lexers.Brainfuck (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Brainfuck"
    , lAliases = ["brainfuck", "bf"]
    , lExtensions = [".bf", ".b"]
    , lMimetypes = ["application/x-brainfuck"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tokNext "\\[" (Arbitrary "Keyword") (GoTo loop')
    , tok "\\]" (Arbitrary "Error")
    , anyOf common'
    ]

common' :: TokenMatcher
common' =
    [ tok "[.,]+" (Arbitrary "Name" :. Arbitrary "Tag")
    , tok "[+-]+" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[<>]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[^.,+\\-<>\\[\\]]+" (Arbitrary "Comment")
    ]

loop' :: TokenMatcher
loop' =
    [ tokNext "\\[" (Arbitrary "Keyword") Push
    , tokNext "\\]" (Arbitrary "Keyword") Pop
    , anyOf common'
    ]

