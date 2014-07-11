module Text.Highlighter.Lexers.Ooc (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Ooc"
    , lAliases = ["ooc"]
    , lExtensions = [".ooc"]
    , lMimetypes = ["text/x-ooc"]
    , lStart = root'
    , lFlags = [multiline]
    }

include' :: TokenMatcher
include' =
    [ tok "[\\w/]+" (Arbitrary "Name")
    , tok "," (Arbitrary "Punctuation")
    , tok "[ \\t]" (Arbitrary "Text")
    , tokNext "[;\\n]" (Arbitrary "Text") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "\\b(class|interface|implement|abstract|extends|from|this|super|new|const|final|static|import|use|extern|inline|proto|break|continue|fallthrough|operator|if|else|for|while|do|switch|case|as|in|version|return|true|false|null)\\b" (Arbitrary "Keyword")
    , tokNext "include\\b" (Arbitrary "Keyword") (GoTo include')
    , tok "(cover)([ \\t]+)(from)([ \\t]+)([a-zA-Z0-9_]+[*@]?)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Class")])
    , tok "(func)((?:[ \\t]|\\\\\\n)+)(\126[a-z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")])
    , tok "\\bfunc\\b" (Arbitrary "Keyword")
    , tok "//.*" (Arbitrary "Comment")
    , tok "(?s)/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "(==?|\\+=?|-[=>]?|\\*=?|/=?|:=|!=?|%=?|\\?|>{1,3}=?|<{1,3}=?|\\.\\.|&&?|\\|\\|?|\\^=?)" (Arbitrary "Operator")
    , tok "(\\.)([ \\t]*)([a-z]\\w*)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")])
    , tok "[A-Z][A-Z0-9_]+" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "[A-Z][a-zA-Z0-9_]*([@*]|\\[[ \\t]*\\])?" (Arbitrary "Name" :. Arbitrary "Class")
    , tok "([a-z][a-zA-Z0-9_]*(?:\126[a-z][a-zA-Z0-9_]*)?)((?:[ \\t]|\\\\\\n)*)(?=\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text")])
    , tok "[a-z][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[:(){}\\[\\];,]" (Arbitrary "Punctuation")
    , tok "0x[0-9a-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0c[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0b[01]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Binary")
    , tok "[0-9_]\\.[0-9_]*(?!\\.)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9_]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Decimal")
    , tok "\"(?:\\\\.|\\\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(?:\\\\.|\\\\[0-9]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\'\\n])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "@" (Arbitrary "Punctuation")
    , tok "\\." (Arbitrary "Punctuation")
    , tok "\\\\[ \\t\\n]" (Arbitrary "Text")
    , tok "[ \\t]+" (Arbitrary "Text")
    ]

