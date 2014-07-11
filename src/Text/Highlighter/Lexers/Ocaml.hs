module Text.Highlighter.Lexers.Ocaml (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "OCaml"
    , lAliases = ["ocaml"]
    , lExtensions = [".ml", ".mli", ".mll", ".mly"]
    , lMimetypes = ["text/x-ocaml"]
    , lStart = root'
    , lFlags = [multiline]
    }

comment' :: TokenMatcher
comment' =
    [ tok "[^(*)]+" (Arbitrary "Comment")
    , tokNext "\\(\\*" (Arbitrary "Comment") Push
    , tokNext "\\*\\)" (Arbitrary "Comment") Pop
    , tok "[(*)]" (Arbitrary "Comment")
    ]

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "false|true|\\(\\)|\\[\\]" (Arbitrary "Name" :. Arbitrary "Builtin" :. Arbitrary "Pseudo")
    , tokNext "\\b([A-Z][A-Za-z0-9_\\']*)(?=\\s*\\.)" (Arbitrary "Name" :. Arbitrary "Namespace") (GoTo dotted')
    , tok "\\b([A-Z][A-Za-z0-9_\\']*)" (Arbitrary "Name" :. Arbitrary "Class")
    , tokNext "\\(\\*" (Arbitrary "Comment") (GoTo comment')
    , tok "\\b(as|assert|begin|class|constraint|do|done|downto|else|end|exception|external|false|for|fun|function|functor|if|in|include|inherit|initializer|lazy|let|match|method|module|mutable|new|object|of|open|private|raise|rec|sig|struct|then|to|true|try|type|val|virtual|when|while|with)\\b" (Arbitrary "Keyword")
    , tok "(!=|#|&|&&|\\(|\\)|\\*|\\+|,|-|-\\.|->|\\.|\\.\\.|:|::|:=|:>|;|;;|<|<-|=|>|>]|>}|\\?|\\?\\?|\\[|\\[<|\\[>|\\[\\||]|_|`|{|{<|\\||\\|]|}|\126)" (Arbitrary "Operator")
    , tok "([=<>@^|&+\\*/$%-]|[!?\126])?[!$%&*+\\./:<=>?@^|\126-]" (Arbitrary "Operator")
    , tok "\\b(and|asr|land|lor|lsl|lxor|mod|or)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "\\b(unit|int|float|bool|string|char|list|array)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "[^\\W\\d][\\w']*" (Arbitrary "Name")
    , tok "\\d[\\d_]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "0[xX][\\da-fA-F][\\da-fA-F_]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[oO][0-7][0-7_]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[bB][01][01_]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Binary")
    , tok "-?\\d[\\d_]*(.[\\d_]*)?([eE][+\\-]?\\d[\\d_]*)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "'(?:(\\\\[\\\\\\\"'ntbr ])|(\\\\[0-9]{3})|(\\\\x[0-9a-fA-F]{2}))'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "'.'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "'" (Arbitrary "Keyword")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo string')
    , tok "[\126?][a-z][\\w\\']*:" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

string' :: TokenMatcher
string' =
    [ tok "[^\\\\\"]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , anyOf escapeSequence'
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") Pop
    ]

escapeSequence' :: TokenMatcher
escapeSequence' =
    [ tok "\\\\[\\\\\\\"\\'ntbr]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "\\\\[0-9]{3}" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "\\\\x[0-9a-fA-F]{2}" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    ]

dotted' :: TokenMatcher
dotted' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "\\." (Arbitrary "Punctuation")
    , tok "[A-Z][A-Za-z0-9_\\']*(?=\\s*\\.)" (Arbitrary "Name" :. Arbitrary "Namespace")
    , tokNext "[A-Z][A-Za-z0-9_\\']*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    , tokNext "[a-z_][A-Za-z0-9_\\']*" (Arbitrary "Name") Pop
    ]

