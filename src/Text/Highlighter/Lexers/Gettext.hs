module Text.Highlighter.Lexers.Gettext (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Gettext Catalog"
    , lAliases = ["pot", "po"]
    , lExtensions = [".pot", ".po"]
    , lMimetypes = ["application/x-gettext", "text/x-gettext", "text/gettext"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "^#,\\s.*?$" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "^#:\\s.*?$" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "^(#|#\\.\\s|#\\|\\s|#\126\\s|#\\s).*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "^(\")([A-Za-z-]+:)(.*\")$" (ByGroups [(Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Name" :. Arbitrary "Property"), (Arbitrary "Literal" :. Arbitrary "String")])
    , tok "^\".*\"$" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "^(msgid|msgid_plural|msgstr)(\\s+)(\".*\")$" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")])
    , tok "^(msgstr\\[)(\\d)(\\])(\\s+)(\".*\")$" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer"), (Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")])
    ]

