module Text.Highlighter.Lexers.Antlr (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "ANTLR"
    , lAliases = ["antlr"]
    , lExtensions = []
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

tokens' :: TokenMatcher
tokens' =
    [ anyOf whitespace'
    , anyOf comments'
    , tok "{" (Arbitrary "Punctuation")
    , tok "([A-Z][A-Za-z_0-9]*)(\\s*)(=)?(\\s*)(\\'(?:\\\\\\\\|\\\\\\'|[^\\']*)\\')?(\\s*)(;)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")])
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

exception' :: TokenMatcher
exception' =
    [ tokNext "\\n" (Arbitrary "Text" :. Arbitrary "Whitespace") Pop
    , tok "\\s" (Arbitrary "Text" :. Arbitrary "Whitespace")
    , anyOf comments'
    , tokNext "\\[" (Arbitrary "Punctuation") (GoTo nestedArgAction')
    , tokNext "\\{" (Arbitrary "Punctuation") (GoTo action')
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\s+" (Arbitrary "Text" :. Arbitrary "Whitespace")
    ]

action' :: TokenMatcher
action' =
    [ tok "([^\\${}\\'\"/\\\\]+|\"(\\\\\\\\|\\\\\"|[^\"])*\"|'(\\\\\\\\|\\\\'|[^'])*'|//.*$\\n?|/\\*(.|\\n)*?\\*/|/(?!\\*)(\\\\\\\\|\\\\/|[^/])*/|\\\\(?!%)|/)+" (Arbitrary "Other")
    , tok "(\\\\)(%)" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Other")])
    , tok "(\\$[a-zA-Z]+)(\\.?)(text|value)?" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Property")])
    , tokNext "{" (Arbitrary "Punctuation") Push
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

nestedArgAction' :: TokenMatcher
nestedArgAction' =
    [ tok "([^\\$\\[\\]\\'\"/]+|\"(\\\\\\\\|\\\\\"|[^\"])*\"|'(\\\\\\\\|\\\\'|[^'])*'|//.*$\\n?|/\\*(.|\\n)*?\\*/|/(?!\\*)(\\\\\\\\|\\\\/|[^/])*/|/)+" (Arbitrary "Other")
    , tokNext "\\[" (Arbitrary "Punctuation") Push
    , tokNext "\\]" (Arbitrary "Punctuation") Pop
    , tok "(\\$[a-zA-Z]+)(\\.?)(text|value)?" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Property")])
    , tok "(\\\\\\\\|\\\\\\]|\\\\\\[|[^\\[\\]])+" (Arbitrary "Other")
    ]

rulePrelims' :: TokenMatcher
rulePrelims' =
    [ anyOf whitespace'
    , anyOf comments'
    , tok "returns\\b" (Arbitrary "Keyword")
    , tokNext "\\[" (Arbitrary "Punctuation") (GoTo nestedArgAction')
    , tokNext "\\{" (Arbitrary "Punctuation") (GoTo action')
    , tok "(throws)(\\s+)([A-Za-z][A-Za-z_0-9]*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Label")])
    , tok "(?:(,)(\\s*)([A-Za-z][A-Za-z_0-9]*))+" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Label")])
    , tokNext "options\\b" (Arbitrary "Keyword") (GoTo options')
    , tokNext "(scope)(\\s+)({)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")]) (GoTo action')
    , tok "(scope)(\\s+)([A-Za-z][A-Za-z_0-9]*)(\\s*)(;)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")])
    , tokNext "(@[A-Za-z][A-Za-z_0-9]*)(\\s*)({)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")]) (GoTo action')
    , tokNext ":" (Arbitrary "Punctuation") Pop
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , anyOf comments'
    , tok "(lexer|parser|tree)?(\\s*)(grammar\\b)(\\s*)([A-Za-z][A-Za-z_0-9]*)(;)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Keyword"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Punctuation")])
    , tokNext "options\\b" (Arbitrary "Keyword") (GoTo options')
    , tokNext "tokens\\b" (Arbitrary "Keyword") (GoTo tokens')
    , tokNext "(scope)(\\s*)([A-Za-z][A-Za-z_0-9]*)(\\s*)({)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")]) (GoTo action')
    , tokNext "(catch|finally)\\b" (Arbitrary "Keyword") (GoTo exception')
    , tokNext "(@[A-Za-z][A-Za-z_0-9]*)(\\s*)(::)?(\\s*)([A-Za-z][A-Za-z_0-9]*)(\\s*)({)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")]) (GoTo action')
    , tokNext "((?:protected|private|public|fragment)\\b)?(\\s*)([A-Za-z][A-Za-z_0-9]*)(!)?" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Punctuation")]) (DoAll [(GoTo ruleAlts'), (GoTo rulePrelims')])
    ]

ruleAlts' :: TokenMatcher
ruleAlts' =
    [ anyOf whitespace'
    , anyOf comments'
    , tokNext "options\\b" (Arbitrary "Keyword") (GoTo options')
    , tok ":" (Arbitrary "Punctuation")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "<<([^>]|>[^>])>>" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\$?[A-Z_][A-Za-z_0-9]*" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "\\$?[a-z_][A-Za-z_0-9]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(\\+|\\||->|=>|=|\\(|\\)|\\.\\.|\\.|\\?|\\*|\\^|!|\\#|\126)" (Arbitrary "Operator")
    , tok "," (Arbitrary "Punctuation")
    , tokNext "\\[" (Arbitrary "Punctuation") (GoTo nestedArgAction')
    , tokNext "\\{" (Arbitrary "Punctuation") (GoTo action')
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

comments' :: TokenMatcher
comments' =
    [ tok "//.*$" (Arbitrary "Comment")
    , tok "/\\*(.|\\n)*?\\*/" (Arbitrary "Comment")
    ]

options' :: TokenMatcher
options' =
    [ anyOf whitespace'
    , anyOf comments'
    , tok "{" (Arbitrary "Punctuation")
    , tok "([A-Za-z][A-Za-z_0-9]*)(\\s*)(=)(\\s*)([A-Za-z][A-Za-z_0-9]*|\\'(?:\\\\\\\\|\\\\\\'|[^\\']*)\\'|[0-9]+|\\*)(\\s*)(;)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Text"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")])
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

