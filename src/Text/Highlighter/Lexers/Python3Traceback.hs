module Text.Highlighter.Lexers.Python3Traceback (lexer) where
import qualified Text.Highlighter.Lexers.Python3 as Python3
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Python 3.0 Traceback"
    , lAliases = ["py3tb"]
    , lExtensions = [".py3tb"]
    , lMimetypes = ["text/x-python3-traceback"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "\\n" (Arbitrary "Text")
    , tokNext "^Traceback \\(most recent call last\\):\\n" (Arbitrary "Generic" :. Arbitrary "Traceback") (GoTo intb')
    , tok "^During handling of the above exception, another exception occurred:\\n\\n" (Arbitrary "Generic" :. Arbitrary "Traceback")
    , tok "^The above exception was the direct cause of the following exception:\\n\\n" (Arbitrary "Generic" :. Arbitrary "Traceback")
    ]

intb' :: TokenMatcher
intb' =
    [ tok "^(  File )(\"[^\"]+\")(, line )(\\d+)(, in )(.+)(\\n)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Text")])
    , tok "^(    )(.+)(\\n)" (ByGroups [(Arbitrary "Text"), (Using Python3.lexer), (Arbitrary "Text")])
    , tok "^([ \\t]*)(...)(\\n)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment"), (Arbitrary "Text")])
    , tokNext "^(.+)(: )(.+)(\\n)" (ByGroups [(Arbitrary "Generic" :. Arbitrary "Error"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Text")]) Pop
    , tokNext "^([a-zA-Z_][a-zA-Z0-9_]*)(:?\\n)" (ByGroups [(Arbitrary "Generic" :. Arbitrary "Error"), (Arbitrary "Text")]) Pop
    ]

