module Text.Highlighter.Lexers.Diff (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Diff"
    , lAliases = ["diff", "udiff"]
    , lExtensions = [".diff", ".patch"]
    , lMimetypes = ["text/x-diff", "text/x-patch"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok " .*\\n" (Arbitrary "Text")
    , tok "\\+.*\\n" (Arbitrary "Generic" :. Arbitrary "Inserted")
    , tok "-.*\\n" (Arbitrary "Generic" :. Arbitrary "Deleted")
    , tok "!.*\\n" (Arbitrary "Generic" :. Arbitrary "Strong")
    , tok "@.*\\n" (Arbitrary "Generic" :. Arbitrary "Subheading")
    , tok "([Ii]ndex|diff).*\\n" (Arbitrary "Generic" :. Arbitrary "Heading")
    , tok "=.*\\n" (Arbitrary "Generic" :. Arbitrary "Heading")
    , tok ".*\\n" (Arbitrary "Text")
    ]

