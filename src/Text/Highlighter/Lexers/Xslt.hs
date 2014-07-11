module Text.Highlighter.Lexers.Xslt (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "XSLT"
    , lAliases = ["xslt"]
    , lExtensions = [".xsl", ".xslt"]
    , lMimetypes = ["text/xml", "application/xml", "image/svg+xml", "application/rss+xml", "application/atom+xml", "application/xsl+xml", "application/xslt+xml"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

comment' :: TokenMatcher
comment' =
    [ tok "[^-]+" (Arbitrary "Comment")
    , tokNext "-->" (Arbitrary "Comment") Pop
    , tok "-" (Arbitrary "Comment")
    ]

tag' :: TokenMatcher
tag' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "[a-zA-Z0-9_.:-]+\\s*=" (Arbitrary "Name" :. Arbitrary "Attribute") (GoTo attr')
    , tokNext "/?\\s*>" (Arbitrary "Name" :. Arbitrary "Tag") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "[^<&]+" (Arbitrary "Text")
    , tok "&\\S*?;" (Arbitrary "Name" :. Arbitrary "Entity")
    , tok "\\<\\!\\[CDATA\\[.*?\\]\\]\\>" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext "<!--" (Arbitrary "Comment") (GoTo comment')
    , tok "<\\?.*?\\?>" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "<![^>]*>" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext "<\\s*[a-zA-Z0-9:._-]+" (Arbitrary "Name" :. Arbitrary "Tag") (GoTo tag')
    , tok "<\\s*/\\s*[a-zA-Z0-9:._-]+\\s*>" (Arbitrary "Name" :. Arbitrary "Tag")
    ]

attr' :: TokenMatcher
attr' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "\".*?\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tokNext "'.*?'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tokNext "[^\\s>]+" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

