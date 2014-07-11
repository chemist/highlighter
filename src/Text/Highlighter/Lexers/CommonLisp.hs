module Text.Highlighter.Lexers.CommonLisp (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Common Lisp"
    , lAliases = ["common-lisp", "cl"]
    , lExtensions = [".cl", ".lisp", ".el"]
    , lMimetypes = ["text/x-common-lisp"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

multilineComment' :: TokenMatcher
multilineComment' =
    [ tokNext "#\\|" (Arbitrary "Comment" :. Arbitrary "Multiline") Push
    , tokNext "\\|#" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tok "[^|#]+" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "[|#]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

body' :: TokenMatcher
body' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok ";.*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "#\\|" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo multilineComment')
    , tok "#\\d*Y.*$" (Arbitrary "Comment" :. Arbitrary "Special")
    , tok "\"(\\\\.|[^\"\\\\])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok ":(\\|[^|]+\\||(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126])(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126]|[#.:])*)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "'(\\|[^|]+\\||(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126])(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126]|[#.:])*)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "'" (Arbitrary "Operator")
    , tok "`" (Arbitrary "Operator")
    , tok "[-+]?\\d+\\.?(?=[ \"()\\'\\n,;`])" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[-+]?\\d+/\\d+(?=[ \"()\\'\\n,;`])" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[-+]?(\\d*\\.\\d+([defls][-+]?\\d+)?|\\d+(\\.\\d*)?[defls][-+]?\\d+)(?=[ \"()\\'\\n,;`])" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "#\\\\.(?=[ \"()\\'\\n,;`])" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "#\\\\(\\|[^|]+\\||(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126])(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126]|[#.:])*)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tokNext "#\\(" (Arbitrary "Operator") (GoTo body')
    , tok "#\\d*\\*[01]*" (Arbitrary "Literal" :. Arbitrary "Other")
    , tok "#:(\\|[^|]+\\||(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126])(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126]|[#.:])*)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "#[.,]" (Arbitrary "Operator")
    , tok "#\\'" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "#[bB][+-]?[01]+(/[01]+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "#[oO][+-]?[0-7]+(/[0-7]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "#[xX][+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "#\\d+[rR][+-]?[0-9a-zA-Z]+(/[0-9a-zA-Z]+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tokNext "(#[cC])(\\()" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Number"), (Arbitrary "Punctuation")]) (GoTo body')
    , tokNext "(#\\d+[aA])(\\()" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Other"), (Arbitrary "Punctuation")]) (GoTo body')
    , tokNext "(#[sS])(\\()" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Other"), (Arbitrary "Punctuation")]) (GoTo body')
    , tok "#[pP]?\"(\\\\.|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "Other")
    , tok "#\\d+=" (Arbitrary "Operator")
    , tok "#\\d+#" (Arbitrary "Operator")
    , tokNext "#+nil(?=[ \"()\\'\\n,;`])\\s*\\(" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo commentedForm')
    , tok "#[+-]" (Arbitrary "Operator")
    , tok "(,@|,|\\.)" (Arbitrary "Operator")
    , tok "(t|nil)(?=[ \"()\\'\\n,;`])" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "\\*(\\|[^|]+\\||(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126])(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126]|[#.:])*)\\*" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Global")
    , tok "(\\|[^|]+\\||(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126])(?:\\\\.|[a-zA-Z0-9!$%&*+-/<=>?@\\[\\]^_{}\126]|[#.:])*)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tokNext "\\(" (Arbitrary "Punctuation") (GoTo body')
    , tokNext "\\)" (Arbitrary "Punctuation") Pop
    ]

root' :: TokenMatcher
root' =
    [ tokNext "" (Arbitrary "Text") (GoTo body')
    ]

commentedForm' :: TokenMatcher
commentedForm' =
    [ tokNext "\\(" (Arbitrary "Comment" :. Arbitrary "Preproc") Push
    , tokNext "\\)" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok "[^()]+" (Arbitrary "Comment" :. Arbitrary "Preproc")
    ]

