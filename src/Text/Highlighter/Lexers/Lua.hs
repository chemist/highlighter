module Text.Highlighter.Lexers.Lua (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Lua"
    , lAliases = ["lua"]
    , lExtensions = [".lua", ".wlua"]
    , lMimetypes = ["text/x-lua", "application/x-lua"]
    , lStart = root'
    , lFlags = [multiline]
    }

classname' :: TokenMatcher
classname' =
    [ tokNext "[A-Za-z_][A-Za-z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

stringescape' :: TokenMatcher
stringescape' =
    [ tok "\\\\([abfnrtv\\\\\"']|\\d{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    ]

base' :: TokenMatcher
base' =
    [ tok "(?s)--\\[(=*)\\[.*?\\]\\1\\]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "--.*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "(?i)(\\d*\\.\\d+|\\d+\\.\\d*)(e[+-]?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(?i)\\d+e[+-]?\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(?i)0x[0-9a-f]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\n" (Arbitrary "Text")
    , tok "[^\\S\\n]" (Arbitrary "Text")
    , tok "(?s)\\[(=*)\\[.*?\\]\\1\\]" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "(==|\126=|<=|>=|\\.\\.|\\.\\.\\.|[=+\\-*/%^<>#])" (Arbitrary "Operator")
    , tok "[\\[\\]\\{\\}\\(\\)\\.,:;]" (Arbitrary "Punctuation")
    , tok "(and|or|not)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "(break|do|else|elseif|end|for|if|in|repeat|return|then|until|while)\\b" (Arbitrary "Keyword")
    , tok "(local)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "(true|false|nil)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tokNext "(function)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo funcname')
    , tokNext "(class)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tok "[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)?" (Arbitrary "Name")
    , tokNext "'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single") (Combined [stringescape', sqs'])
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (Combined [stringescape', dqs'])
    ]

string' :: TokenMatcher
string' =
    [ tok "." (Arbitrary "Literal" :. Arbitrary "String")
    ]

dqs' :: TokenMatcher
dqs' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf string'
    ]

sqs' :: TokenMatcher
sqs' =
    [ tokNext "'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf string'
    ]

root' :: TokenMatcher
root' =
    [ tok "#!(.*?)$" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext "" (Arbitrary "Text") (GoTo base')
    ]

funcname' :: TokenMatcher
funcname' =
    [ tokNext "(?:([A-Za-z_][A-Za-z0-9_]*)(\\.))?([A-Za-z_][A-Za-z0-9_]*)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Function")]) Pop
    , tokNext "\\(" (Arbitrary "Punctuation") Pop
    ]

