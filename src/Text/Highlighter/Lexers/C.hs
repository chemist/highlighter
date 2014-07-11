module Text.Highlighter.Lexers.C (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "C"
    , lAliases = ["c"]
    , lExtensions = [".c", ".h"]
    , lMimetypes = ["text/x-chdr", "text/x-csrc"]
    , lStart = root'
    , lFlags = [multiline]
    }

function' :: TokenMatcher
function' =
    [ anyOf whitespace'
    , anyOf statements'
    , tok ";" (Arbitrary "Punctuation")
    , tokNext "{" (Arbitrary "Punctuation") Push
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

statements' :: TokenMatcher
statements' =
    [ tokNext "L?\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "L?'(\\\\.|\\\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\'\\n])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+)[eE][+-]?\\d+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-fA-F]+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\d+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\*/" (Arbitrary "Error")
    , tok "[\126!%^&*+=|?:<>/-]" (Arbitrary "Operator")
    , tok "[()\\[\\],.]" (Arbitrary "Punctuation")
    , tok "\\b(case)(.+?)(:)" (ByGroups [(Arbitrary "Keyword"), (Using lexer), (Arbitrary "Text")])
    , tok "(auto|break|case|const|continue|default|do|else|enum|extern|for|goto|if|register|restricted|return|sizeof|static|struct|switch|typedef|union|volatile|virtual|while)\\b" (Arbitrary "Keyword")
    , tok "(int|long|float|short|double|char|unsigned|signed|void)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(_{0,2}inline|naked|restrict|thread|typename)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|declspec|finally|int64|try|leave)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(true|false|NULL)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tokNext "^#if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tokNext "^#" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    , tokNext "^(?:\\s|//.*?\\n|/[*].*?[*]/)+#if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tokNext "^(?:\\s|//.*?\\n|/[*].*?[*]/)+#" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    , tok "^(\\s*)([a-zA-Z_][a-zA-Z0-9_]*:(?!:))" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label")])
    , tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "//(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

statement' :: TokenMatcher
statement' =
    [ anyOf whitespace'
    , anyOf statements'
    , tok "[{}]" (Arbitrary "Punctuation")
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

if0' :: TokenMatcher
if0' =
    [ tokNext "^\\s*#if.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Push
    , tokNext "^\\s*#el(?:se|if).*\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tokNext "^\\s*#endif.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok ".*?\\n" (Arbitrary "Comment")
    ]

macro' :: TokenMatcher
macro' =
    [ tok "[^/\\n]+" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "/[*](.|\\n)*?[*]/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single") Pop
    , tok "/" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "(?<=\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext "\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tokNext "((?:[a-zA-Z0-9_*\\s])+?(?:\\s|[*]))([a-zA-Z_][a-zA-Z0-9_]*)(\\s*\\([^;]*?\\))((?:\\s|//.*?\\n|/[*].*?[*]/)+)({)" (ByGroups [(Using lexer), (Arbitrary "Name" :. Arbitrary "Function"), (Using lexer), (Using lexer), (Arbitrary "Punctuation")]) (GoTo function')
    , tok "((?:[a-zA-Z0-9_*\\s])+?(?:\\s|[*]))([a-zA-Z_][a-zA-Z0-9_]*)(\\s*\\([^;]*?\\))((?:\\s|//.*?\\n|/[*].*?[*]/)+)(;)" (ByGroups [(Using lexer), (Arbitrary "Name" :. Arbitrary "Function"), (Using lexer), (Using lexer), (Arbitrary "Punctuation")])
    , tokNext "" (Arbitrary "Text") (GoTo statement')
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

