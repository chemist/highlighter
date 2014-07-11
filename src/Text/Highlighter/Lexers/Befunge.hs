module Text.Highlighter.Lexers.Befunge (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Befunge"
    , lAliases = ["befunge"]
    , lExtensions = [".befunge"]
    , lMimetypes = ["application/x-befunge"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "[0-9a-f]" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[\\+\\*/%!`-]" (Arbitrary "Operator")
    , tok "[<>^v?\\[\\]rxjk]" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[:\\\\$.,n]" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[|_mw]" (Arbitrary "Keyword")
    , tok "[{}]" (Arbitrary "Name" :. Arbitrary "Tag")
    , tok "\".*?\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "\\'." (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "[#;]" (Arbitrary "Comment")
    , tok "[pg&\126=@iotsy]" (Arbitrary "Keyword")
    , tok "[()A-Z]" (Arbitrary "Comment")
    , tok "\\s+" (Arbitrary "Text")
    ]

