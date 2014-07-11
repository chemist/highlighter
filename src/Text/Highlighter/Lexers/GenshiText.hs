module Text.Highlighter.Lexers.GenshiText (lexer) where
import qualified Text.Highlighter.Lexers.Python as Python
import qualified Text.Highlighter.Lexers.Python as Python
import qualified Text.Highlighter.Lexers.Python as Python
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Genshi Text"
    , lAliases = ["genshitext"]
    , lExtensions = []
    , lMimetypes = ["application/x-genshi-text", "text/x-genshi"]
    , lStart = root'
    , lFlags = [multiline]
    }

variable' :: TokenMatcher
variable' =
    [ tok "(?<!\\$)(\\$\\{)(.+?)(\\})" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Using Python.lexer), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tok "(?<!\\$)(\\$)([a-zA-Z_][a-zA-Z0-9_\\.]*)" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

root' :: TokenMatcher
root' =
    [ tok "[^#\\$\\s]+" (Arbitrary "Other")
    , tok "^(\\s*)(##.*)$" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment")])
    , tokNext "^(\\s*)(#)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc")]) (GoTo directive')
    , anyOf variable'
    , tok "[#\\$\\s]" (Arbitrary "Other")
    ]

directive' :: TokenMatcher
directive' =
    [ tokNext "\\n" (Arbitrary "Text") Pop
    , tokNext "(?:def|for|if)\\s+.*" (Using Python.lexer) Pop
    , tokNext "(choose|when|with)([^\\S\\n]+)(.*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Using Python.lexer)]) Pop
    , tokNext "(choose|otherwise)\\b" (Arbitrary "Keyword") Pop
    , tokNext "(end\\w*)([^\\S\\n]*)(.*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Comment")]) Pop
    ]

