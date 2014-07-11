module Text.Highlighter.Lexers.Dylan (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Dylan"
    , lAliases = ["dylan"]
    , lExtensions = [".dylan", ".dyl"]
    , lMimetypes = ["text/x-dylan"]
    , lStart = root'
    , lFlags = [dotall]
    }

root' :: TokenMatcher
root' =
    [ tok "\\b(subclass|abstract|block|c(on(crete|stant)|lass)|domain|ex(c(eption|lude)|port)|f(unction(|al))|generic|handler|i(n(herited|line|stance|terface)|mport)|library|m(acro|ethod)|open|primary|sealed|si(deways|ngleton)|slot|v(ariable|irtual))\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "<\\w+>" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*[\\w\\W]*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "'(\\\\.|\\\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\'\\n])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "=>|\\b(a(bove|fterwards)|b(e(gin|low)|y)|c(ase|leanup|reate)|define|else(|if)|end|f(inally|or|rom)|i[fn]|l(et|ocal)|otherwise|rename|s(elect|ignal)|t(hen|o)|u(n(less|til)|se)|wh(en|ile))\\b" (Arbitrary "Keyword")
    , tok "([ \\t])([!\\$%&\\*\\/:<=>\\?\126_^a-zA-Z0-9.+\\-]*:)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable")])
    , tok "([ \\t]*)(\\S+[^:])([ \\t]*)(\\()([ \\t]*)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Text")])
    , tok "-?[0-9.]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[(),;]" (Arbitrary "Punctuation")
    , tok "\\$[a-zA-Z0-9-]+" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "[!$%&*/:<>=?\126^.+\\[\\]{}-]+" (Arbitrary "Operator")
    , tok "\\s+" (Arbitrary "Text")
    , tok "#[a-zA-Z0-9-]+" (Arbitrary "Keyword")
    , tok "[a-zA-Z0-9-]+" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

