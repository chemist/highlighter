module Text.Highlighter.Lexers.S (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "S"
    , lAliases = ["splus", "s", "r"]
    , lExtensions = [".S", ".R"]
    , lMimetypes = ["text/S-plus", "text/S", "text/R"]
    , lStart = root'
    , lFlags = [multiline]
    }

statements' :: TokenMatcher
statements' =
    [ anyOf comments'
    , tok "\\s+" (Arbitrary "Text")
    , tokNext "\\'" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string_squote')
    , tokNext "\\\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string_dquote')
    , anyOf builtin_symbols'
    , anyOf numbers'
    , anyOf keywords'
    , anyOf punctuation'
    , anyOf operators'
    , anyOf valid_name'
    ]

string_dquote' :: TokenMatcher
string_dquote' =
    [ tokNext "[^\\\"]*\\\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

operators' :: TokenMatcher
operators' =
    [ tok "<-|-|==|<=|>=|<|>|&&|&|!=|\\|\\|?" (Arbitrary "Operator")
    , tok "\\*|\\+|\\^|/|%%|%/%|=" (Arbitrary "Operator")
    , tok "%in%|%*%" (Arbitrary "Operator")
    ]

keywords' :: TokenMatcher
keywords' =
    [ tok "for(?=\\s*\\()|while(?=\\s*\\()|if(?=\\s*\\()|(?<=\\s)else|(?<=\\s)break(?=;|$)|return(?=\\s*\\()|function(?=\\s*\\()" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    ]

numbers' :: TokenMatcher
numbers' =
    [ tok "(?<![0-9a-zA-Z\\)\\}\\]`\\\"])(?=\\s*)[-\\+]?[0-9]+(\\.[0-9]*)?(E[0-9][-\\+]?(\\.[0-9]*)?)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\.[0-9]*(E[0-9][-\\+]?(\\.[0-9]*)?)?" (Arbitrary "Literal" :. Arbitrary "Number")
    ]

valid_name' :: TokenMatcher
valid_name' =
    [ tok "[a-zA-Z][0-9a-zA-Z\\._]+" (Arbitrary "Text")
    , tok "`.+`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick")
    ]

builtin_symbols' :: TokenMatcher
builtin_symbols' =
    [ tok "(NULL|NA|TRUE|FALSE|NaN)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(T|F)\\b" (Arbitrary "Keyword" :. Arbitrary "Variable")
    ]

punctuation' :: TokenMatcher
punctuation' =
    [ tok "\\[|\\]|\\[\\[|\\]\\]|\\$|\\(|\\)|@|:::?|;|," (Arbitrary "Punctuation")
    ]

comments' :: TokenMatcher
comments' =
    [ tok "#.*$" (Arbitrary "Comment" :. Arbitrary "Single")
    ]

root' :: TokenMatcher
root' =
    [ anyOf statements'
    , tok "\\{|\\}" (Arbitrary "Punctuation")
    , tok "." (Arbitrary "Text")
    ]

string_squote' :: TokenMatcher
string_squote' =
    [ tokNext "[^\\']*\\'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

