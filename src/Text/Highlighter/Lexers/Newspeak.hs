module Text.Highlighter.Lexers.Newspeak (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Newspeak"
    , lAliases = ["newspeak"]
    , lExtensions = [".ns2"]
    , lMimetypes = ["text/x-newspeak"]
    , lStart = root'
    , lFlags = [multiline]
    }

literals' :: TokenMatcher
literals' =
    [ tok "\\$." (Arbitrary "Literal" :. Arbitrary "String")
    , tok "'[^']*'" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "#'[^']*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "#\\w+:?" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "#(\\+|\\/|\126|\\*|<|>|=|@|%|\\||&|\\?|!|,|-)+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    ]

root' :: TokenMatcher
root' =
    [ tok "\\b(Newsqueak2)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "'[^']*'" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\b(class)(\\s+)([a-zA-Z0-9_]+)(\\s*)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text")])
    , tok "\\b(mixin|self|super|private|public|protected|nil|true|false)\\b" (Arbitrary "Keyword")
    , tok "([a-zA-Z0-9_]+\\:)(\\s*)([a-zA-Z_]\\w+)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable")])
    , tok "([a-zA-Z0-9_]+)(\\s*)(=)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Attribute"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tok "<[a-zA-Z0-9_]+>" (Arbitrary "Comment" :. Arbitrary "Special")
    , anyOf expressionstat'
    , anyOf whitespace'
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "\"[^\"]*\"" (Arbitrary "Comment")
    ]

expressionstat' :: TokenMatcher
expressionstat' =
    [ tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok ":\\w+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(\\w+)(::)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Operator")])
    , tok "\\w+:" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "\\w+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "\\(|\\)" (Arbitrary "Punctuation")
    , tok "\\[|\\]" (Arbitrary "Punctuation")
    , tok "\\{|\\}" (Arbitrary "Punctuation")
    , tok "(\\^|\\+|\\/|\126|\\*|<|>|=|@|%|\\||&|\\?|!|,|-|:)" (Arbitrary "Operator")
    , tok "\\.|;" (Arbitrary "Punctuation")
    , anyOf whitespace'
    , anyOf literals'
    ]

