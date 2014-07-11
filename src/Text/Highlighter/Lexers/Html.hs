module Text.Highlighter.Lexers.Html (lexer) where
import qualified Text.Highlighter.Lexers.Css as Css
import qualified Text.Highlighter.Lexers.Javascript as Javascript
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "HTML"
    , lAliases = ["html"]
    , lExtensions = [".html", ".htm", ".xhtml", ".xslt"]
    , lMimetypes = ["text/html", "application/xhtml+xml"]
    , lStart = root'
    , lFlags = [caseless, dotall]
    }

comment' :: TokenMatcher
comment' =
    [ tok "[^-]+" (Arbitrary "Comment")
    , tokNext "-->" (Arbitrary "Comment") Pop
    , tok "-" (Arbitrary "Comment")
    ]

styleContent' :: TokenMatcher
styleContent' =
    [ tokNext "<\\s*/\\s*style\\s*>" (Arbitrary "Name" :. Arbitrary "Tag") Pop
    , tok ".+?(?=<\\s*/\\s*style\\s*>)" (Using Css.lexer)
    ]

attr' :: TokenMatcher
attr' =
    [ tokNext "\".*?\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tokNext "'.*?'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tokNext "[^\\s>]+" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

scriptContent' :: TokenMatcher
scriptContent' =
    [ tokNext "<\\s*/\\s*script\\s*>" (Arbitrary "Name" :. Arbitrary "Tag") Pop
    , tok ".+?(?=<\\s*/\\s*script\\s*>)" (Using Javascript.lexer)
    ]

tag' :: TokenMatcher
tag' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "[a-zA-Z0-9_:-]+\\s*=" (Arbitrary "Name" :. Arbitrary "Attribute") (GoTo attr')
    , tok "[a-zA-Z0-9_:-]+" (Arbitrary "Name" :. Arbitrary "Attribute")
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
    , tokNext "<\\s*script\\s*" (Arbitrary "Name" :. Arbitrary "Tag") (DoAll [(GoTo scriptContent'), (GoTo tag')])
    , tokNext "<\\s*style\\s*" (Arbitrary "Name" :. Arbitrary "Tag") (DoAll [(GoTo styleContent'), (GoTo tag')])
    , tokNext "<\\s*[a-zA-Z0-9:]+" (Arbitrary "Name" :. Arbitrary "Tag") (GoTo tag')
    , tok "<\\s*/\\s*[a-zA-Z0-9:]+\\s*>" (Arbitrary "Name" :. Arbitrary "Tag")
    ]

