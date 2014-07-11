module Text.Highlighter.Lexers.MoinWiki (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "MoinMoin/Trac Wiki markup"
    , lAliases = ["trac-wiki", "moin"]
    , lExtensions = []
    , lMimetypes = ["text/x-trac-wiki"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "^#.*$" (Arbitrary "Comment")
    , tok "(!)(\\S+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")])
    , tok "^(=+)([^=]+)(=+)(\\s*#.+)?$" (ByGroups [(Arbitrary "Generic" :. Arbitrary "Heading"), (Using lexer), (Arbitrary "Generic" :. Arbitrary "Heading"), (Arbitrary "Literal" :. Arbitrary "String")])
    , tokNext "({{{)(\\n#!.+)?" (ByGroups [(Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Name" :. Arbitrary "Namespace")]) (GoTo codeblock')
    , tok "(\\'\\'\\'?|\\|\\||`|__|\126\126|\\^|,,|::)" (Arbitrary "Comment")
    , tok "^( +)([.*-])( )" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Text")])
    , tok "^( +)([a-zivx]{1,5}\\.)( )" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Text")])
    , tok "\\[\\[\\w+.*?\\]\\]" (Arbitrary "Keyword")
    , tok "(\\[[^\\s\\]]+)(\\s+[^\\]]+?)?(\\])" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Keyword")])
    , tok "^----+$" (Arbitrary "Keyword")
    , tok "[^\\n\\'\\[{!_\126^,|]+" (Arbitrary "Text")
    , tok "\\n" (Arbitrary "Text")
    , tok "." (Arbitrary "Text")
    ]

codeblock' :: TokenMatcher
codeblock' =
    [ tokNext "}}}" (Arbitrary "Name" :. Arbitrary "Builtin") Pop
    , tokNext "{{{" (Arbitrary "Text") Push
    , tok "[^{}]+" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "." (Arbitrary "Comment" :. Arbitrary "Preproc")
    ]

