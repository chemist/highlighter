module Text.Highlighter.Lexers.PythonTraceback (lexer) where
import qualified Text.Highlighter.Lexers.Python as Python
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Python Traceback"
    , lAliases = ["pytb"]
    , lExtensions = [".pytb"]
    , lMimetypes = ["text/x-python-traceback"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tokNext "^Traceback \\(most recent call last\\):\\n" (Arbitrary "Generic" :. Arbitrary "Traceback") (GoTo intb')
    , tokNext "^(?=  File \"[^\"]+\", line \\d+)" (Arbitrary "Generic" :. Arbitrary "Traceback") (GoTo intb')
    , tok "^.*\\n" (Arbitrary "Other")
    ]

intb' :: TokenMatcher
intb' =
    [ tok "^(  File )(\"[^\"]+\")(, line )(\\d+)(, in )(.+)(\\n)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Text")])
    , tok "^(  File )(\"[^\"]+\")(, line )(\\d+)(\\n)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number"), (Arbitrary "Text")])
    , tok "^(    )(.+)(\\n)" (ByGroups [(Arbitrary "Text"), (Using Python.lexer), (Arbitrary "Text")])
    , tok "^([ \\t]*)(...)(\\n)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment"), (Arbitrary "Text")])
    , tokNext "^(.+)(: )(.+)(\\n)" (ByGroups [(Arbitrary "Generic" :. Arbitrary "Error"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Text")]) Pop
    , tokNext "^([a-zA-Z_][a-zA-Z0-9_]*)(:?\\n)" (ByGroups [(Arbitrary "Generic" :. Arbitrary "Error"), (Arbitrary "Text")]) Pop
    ]

