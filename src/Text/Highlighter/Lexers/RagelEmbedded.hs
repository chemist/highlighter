module Text.Highlighter.Lexers.RagelEmbedded (lexer) where
import qualified Text.Highlighter.Lexers.Ragel as Ragel
import qualified Text.Highlighter.Lexers.Ragel as Ragel
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Embedded Ragel"
    , lAliases = ["ragel-em"]
    , lExtensions = [".rl"]
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "([^%\\'\"/#]+|%(?=[^%]|$)|\"(\\\\\\\\|\\\\\"|[^\"])*\"|'(\\\\\\\\|\\\\'|[^'])*'|/\\*(.|\\n)*?\\*/|//.*$\\n?|\\#.*$\\n?|/(?!\\*)(\\\\\\\\|\\\\/|[^/])*/|/)+" (Arbitrary "Other")
    , tok "(%%)(?![{%])(.*)($|;)(\\n?)" (ByGroups [(Arbitrary "Punctuation"), (Using Ragel.lexer), (Arbitrary "Punctuation"), (Arbitrary "Text")])
    , tokNext "(%%%%|%%){" (Arbitrary "Punctuation") (GoTo multiLineFsm')
    ]

multiLineFsm' :: TokenMatcher
multiLineFsm' =
    [ tok "(([^}\\'\"\\[/#]|}(?=[^%]|$)|}%(?=[^%]|$)|[^\\\\][\\\\][{}]|(>|\\$|%|<|@|<>)/|/(?!\\*)(\\\\\\\\|\\\\/|[^/])*/\\*|/(?=[^/\\*]|$))+|\"(\\\\\\\\|\\\\\"|[^\"])*\"|'(\\\\\\\\|\\\\'|[^'])*'|\\[(\\\\\\\\|\\\\\\]|[^\\]])*\\]|/\\*(.|\\n)*?\\*/|//.*$\\n?|\\#.*$\\n?)+" (Using Ragel.lexer)
    , tokNext "}%%" (Arbitrary "Punctuation") Pop
    ]

