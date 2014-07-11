module Text.Highlighter.Lexers.Django (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Django/Jinja"
    , lAliases = ["django", "jinja"]
    , lExtensions = []
    , lMimetypes = ["application/x-django-templating", "application/x-jinja"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

var' :: TokenMatcher
var' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "(-?)(\\}\\})" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc")]) Pop
    , anyOf varnames'
    ]

varnames' :: TokenMatcher
varnames' =
    [ tok "(\\|)(\\s*)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")])
    , tok "(is)(\\s+)(not)?(\\s+)?([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")])
    , tok "(_|true|false|none|True|False|None)\\b" (Arbitrary "Keyword" :. Arbitrary "Pseudo")
    , tok "(in|as|reversed|recursive|not|and|or|is|if|else|import|with(?:(?:out)?\\s*context)?|scoped|ignore\\s+missing)\\b" (Arbitrary "Keyword")
    , tok "(loop|block|super|forloop)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[a-zA-Z][a-zA-Z0-9_-]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "\\.[a-zA-Z0-9_]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok ":?\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok ":?'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "([{}()\\[\\]+\\-*/,:\126]|[><=]=?)" (Arbitrary "Operator")
    , tok "[0-9](\\.[0-9]*)?(eE[+-][0-9])?[flFLdD]?|0[xX][0-9a-fA-F]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number")
    ]

root' :: TokenMatcher
root' =
    [ tok "[^{]+" (Arbitrary "Other")
    , tokNext "\\{\\{" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo var')
    , tok "\\{[*#].*?[*#]\\}" (Arbitrary "Comment")
    , tok "(\\{%)(-?\\s*)(comment)(\\s*-?)(%\\})(.*?)(\\{%)(-?\\s*)(endcomment)(\\s*-?)(%\\})" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Comment"), (Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tok "(\\{%)(-?\\s*)(raw)(\\s*-?)(%\\})(.*?)(\\{%)(-?\\s*)(endraw)(\\s*-?)(%\\})" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tokNext "(\\{%)(-?\\s*)(filter)(\\s+)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")]) (GoTo block')
    , tokNext "(\\{%)(-?\\s*)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Keyword")]) (GoTo block')
    , tok "\\{" (Arbitrary "Other")
    ]

block' :: TokenMatcher
block' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "(-?)(%\\})" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc")]) Pop
    , anyOf varnames'
    , tok "." (Arbitrary "Punctuation")
    ]

