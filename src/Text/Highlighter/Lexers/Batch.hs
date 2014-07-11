module Text.Highlighter.Lexers.Batch (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Batchfile"
    , lAliases = ["bat"]
    , lExtensions = [".bat", ".cmd"]
    , lMimetypes = ["application/x-dos-batch"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

basic' :: TokenMatcher
basic' =
    [ tok "\".*?\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'.*?'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "`.*?`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick")
    , tok "-?\\d+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "," (Arbitrary "Punctuation")
    , tok "=" (Arbitrary "Operator")
    , tok "/\\S+" (Arbitrary "Name")
    , tok ":\\w+" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "\\w:\\w+" (Arbitrary "Text")
    , tok "([<>|])(\\s*)(\\w+)" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Name")])
    ]

root' :: TokenMatcher
root' =
    [ tok "^\\s*@" (Arbitrary "Punctuation")
    , tok "^(\\s*)(rem\\s.*)$" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment")])
    , tok "\".*?\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'.*?'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "%%?[\126$:\\w]+%?" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "::.*" (Arbitrary "Comment")
    , tok "(set)(\\s+)(\\w+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable")])
    , tok "(call)(\\s+)(:\\w+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label")])
    , tok "(goto)(\\s+)(\\w+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label")])
    , tok "\\b(set|call|echo|on|off|endlocal|for|do|goto|if|pause|setlocal|shift|errorlevel|exist|defined|cmdextversion|errorlevel|else|cd|md|del|deltree|cls|choice)\\b" (Arbitrary "Keyword")
    , tok "\\b(equ|neq|lss|leq|gtr|geq)\\b" (Arbitrary "Operator")
    , anyOf basic'
    , tok "." (Arbitrary "Text")
    ]

echo' :: TokenMatcher
echo' =
    [ tok "\\^\\^|\\^<|\\^>|\\^\\|" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tokNext "\\n" (Arbitrary "Text") Pop
    , anyOf basic'
    , tok "[^\\'\"^]+" (Arbitrary "Text")
    ]

