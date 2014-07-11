module Text.Highlighter.Lexers.ApacheConf (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "ApacheConf"
    , lAliases = ["apacheconf", "aconf", "apache"]
    , lExtensions = [".htaccess", "apache.conf", "apache2.conf"]
    , lMimetypes = ["text/x-apacheconf"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "(#.*?)$" (Arbitrary "Comment")
    , tok "(<[^\\s>]+)(?:(\\s+)(.*?))?(>)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Tag"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Name" :. Arbitrary "Tag")])
    , tokNext "([a-zA-Z][a-zA-Z0-9]*)(\\s+)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Text")]) (GoTo value')
    , tok "\\.+" (Arbitrary "Text")
    ]

value' :: TokenMatcher
value' =
    [ tokNext "$" (Arbitrary "Text") Pop
    , tok "[^\\S\\n]+" (Arbitrary "Text")
    , tok "\\d+\\.\\d+\\.\\d+\\.\\d+(?:/\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "/([a-zA-Z0-9][a-zA-Z0-9_./-]+)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tok "(on|off|none|any|all|double|email|dns|min|minimal|os|productonly|full|emerg|alert|crit|error|warn|notice|info|debug|registry|script|inetd|standalone|user|group)\\b" (Arbitrary "Keyword")
    , tok "\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "[^\\s\"]+" (Arbitrary "Text")
    ]

