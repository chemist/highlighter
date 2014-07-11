module Text.Highlighter.Lexers.DarcsPatch (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Darcs Patch"
    , lAliases = ["dpatch"]
    , lExtensions = [".dpatch", ".darcspatch"]
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

comment' :: TokenMatcher
comment' =
    [ tok "[^\\]].*\\n" (Arbitrary "Comment")
    , tokNext "\\]" (Arbitrary "Operator") Pop
    ]

insert' :: TokenMatcher
insert' =
    [ anyOf specialText'
    , tok "\\[" (Arbitrary "Generic" :. Arbitrary "Inserted")
    , tok "[^\\n\\[]*" (Arbitrary "Generic" :. Arbitrary "Inserted")
    ]

delete' :: TokenMatcher
delete' =
    [ anyOf specialText'
    , tok "\\[" (Arbitrary "Generic" :. Arbitrary "Deleted")
    , tok "[^\\n\\[]*" (Arbitrary "Generic" :. Arbitrary "Deleted")
    ]

root' :: TokenMatcher
root' =
    [ tok "<" (Arbitrary "Operator")
    , tok ">" (Arbitrary "Operator")
    , tok "{" (Arbitrary "Operator")
    , tok "}" (Arbitrary "Operator")
    , tok "(\\[)((?:TAG )?)(.*)(\\n)(.*)(\\*\\*)(\\d+)(\\s?)(\\])" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Keyword"), (Arbitrary "Name"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Operator"), (Arbitrary "Literal" :. Arbitrary "Date"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tokNext "(\\[)((?:TAG )?)(.*)(\\n)(.*)(\\*\\*)(\\d+)(\\s?)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Keyword"), (Arbitrary "Name"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Operator"), (Arbitrary "Literal" :. Arbitrary "Date"), (Arbitrary "Text")]) (GoTo comment')
    , tok "New patches:" (Arbitrary "Generic" :. Arbitrary "Heading")
    , tok "Context:" (Arbitrary "Generic" :. Arbitrary "Heading")
    , tok "Patch bundle hash:" (Arbitrary "Generic" :. Arbitrary "Heading")
    , tok "(\\s*)(hunk|addfile|adddir|rmfile|rmdir|move|replace)(.*\\n)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text")])
    , tokNext "\\+" (Arbitrary "Generic" :. Arbitrary "Inserted") (GoTo insert')
    , tokNext "-" (Arbitrary "Generic" :. Arbitrary "Deleted") (GoTo delete')
    , tok ".*\\n" (Arbitrary "Text")
    ]

specialText' :: TokenMatcher
specialText' =
    [ tokNext "\\n" (Arbitrary "Text") Pop
    , tok "\\[_[^_]*_]" (Arbitrary "Operator")
    ]

