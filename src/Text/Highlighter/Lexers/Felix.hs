module Text.Highlighter.Lexers.Felix (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Felix"
    , lAliases = ["felix", "flx"]
    , lExtensions = [".flx", ".flxh"]
    , lMimetypes = ["text/x-felix"]
    , lStart = root'
    , lFlags = [multiline]
    }

comment' :: TokenMatcher
comment' =
    [ tok "//(.*?)\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "/[*]" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo comment2')
    ]

modulename2' :: TokenMatcher
modulename2' =
    [ anyOf whitespace'
    , tokNext "([a-zA-Z_]\\w*)" (Arbitrary "Name" :. Arbitrary "Namespace") (PopNum 2)
    ]

modulename' :: TokenMatcher
modulename' =
    [ anyOf whitespace'
    , tokNext "\\[" (Arbitrary "Punctuation") (DoAll [(GoTo modulename2'), (GoTo tvarlist')])
    , tokNext "" (Arbitrary "Error") (GoTo modulename2')
    ]

if0' :: TokenMatcher
if0' =
    [ tokNext "^\\s*#if.*?(?<!\\\\)\\n" (Arbitrary "Comment") Push
    , tokNext "^\\s*#endif.*?(?<!\\\\)\\n" (Arbitrary "Comment") Pop
    , tok ".*?\\n" (Arbitrary "Comment")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , anyOf comment'
    , tokNext "#\\s*if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tokNext "#" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    ]

dqs' :: TokenMatcher
dqs' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\\\\\|\\\\\"|\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , anyOf strings'
    ]

sqs' :: TokenMatcher
sqs' =
    [ tokNext "'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\\\\\|\\\\'|\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , anyOf strings'
    ]

macro' :: TokenMatcher
macro' =
    [ anyOf comment'
    , tokNext "(import|include)(\\s+)(<[^>]*?>)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")]) Pop
    , tokNext "(import|include)(\\s+)(\"[^\"]*?\")" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")]) Pop
    , tokNext "(import|include)(\\s+)('[^']*?')" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")]) Pop
    , tok "[^/\\n]+" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "/" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "(?<=\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext "\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    ]

operators' :: TokenMatcher
operators' =
    [ tok "(and|not|in|is|isin|or|xor)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "!=|==|<<|>>|\\|\\||&&|[-\126+/*%=<>&^|.$]" (Arbitrary "Operator")
    ]

tsqs' :: TokenMatcher
tsqs' =
    [ tokNext "'''" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf strings'
    , anyOf nl'
    ]

comment2' :: TokenMatcher
comment2' =
    [ tok "[^\\/*]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "/[*]" (Arbitrary "Comment" :. Arbitrary "Multiline") Push
    , tokNext "[*]/" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tok "[\\/*]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

classname' :: TokenMatcher
classname' =
    [ anyOf whitespace'
    , tokNext "[a-zA-Z_]\\w*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    , tokNext "(?=\\{)" (Arbitrary "Text") Pop
    ]

stringescape' :: TokenMatcher
stringescape' =
    [ tok "\\\\([\\\\abfnrtv\"\\']|\\n|N{.*?}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    ]

tdqs' :: TokenMatcher
tdqs' =
    [ tokNext "\"\"\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf strings'
    , anyOf nl'
    ]

nl' :: TokenMatcher
nl' =
    [ tok "\\n" (Arbitrary "Literal" :. Arbitrary "String")
    ]

funcname' :: TokenMatcher
funcname' =
    [ anyOf whitespace'
    , tokNext "[a-zA-Z_]\\w*" (Arbitrary "Name" :. Arbitrary "Function") Pop
    , tokNext "(?=\\()" (Arbitrary "Text") Pop
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tokNext "(axiom|ctor|fun|gen|proc|reduce|union)\\b" (Arbitrary "Keyword") (GoTo funcname')
    , tokNext "(class|cclass|cstruct|obj|struct)\\b" (Arbitrary "Keyword") (GoTo classname')
    , tokNext "(instance|module|typeclass)\\b" (Arbitrary "Keyword") (GoTo modulename')
    , tok "(_|_deref|all|as|assert|attempt|call|callback|case|caseno|cclass|code|compound|ctypes|do|done|downto|elif|else|endattempt|endcase|endif|endmatch|enum|except|exceptions|expect|finally|for|forall|forget|fork|functor|goto|ident|if|incomplete|inherit|instance|interface|jump|lambda|loop|match|module|namespace|new|noexpand|nonterm|obj|of|open|parse|raise|regexp|reglex|regmatch|rename|return|the|then|to|type|typecase|typedef|typematch|typeof|upto|when|whilst|with|yield)\\b" (Arbitrary "Keyword")
    , tok "(_gc_pointer|_gc_type|body|comment|const|export|header|inline|lval|macro|noinline|noreturn|package|private|pod|property|public|publish|requires|todo|virtual|use)\\b" (Arbitrary "Name" :. Arbitrary "Decorator")
    , tok "(def|let|ref|val|var)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "(unit|void|any|bool|byte|offset|address|caddress|cvaddress|vaddress|tiny|short|int|long|vlong|utiny|ushort|vshort|uint|ulong|uvlong|int8|int16|int32|int64|uint8|uint16|uint32|uint64|float|double|ldouble|complex|dcomplex|lcomplex|imaginary|dimaginary|limaginary|char|wchar|uchar|charp|charcp|ucharp|ucharcp|string|wstring|ustring|cont|array|varray|list|lvalue|opt|slice)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(false|true)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , anyOf operators'
    , tok "0[xX]([0-9a-fA-F_]*\\.[0-9a-fA-F_]+|[0-9a-fA-F_]+)[pP][+\\-]?[0-9_]+[lLfFdD]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9_]+(\\.[0-9_]+[eE][+\\-]?[0-9_]+|\\.[0-9_]*|[eE][+\\-]?[0-9_]+)[lLfFdD]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\.(0|[1-9][0-9_]*)([eE][+\\-]?[0-9_]+)?[lLfFdD]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[Bb][01_]+([tTsSiIlLvV]|ll|LL|([iIuU])(8|16|32|64))?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0[0-7_]+([tTsSiIlLvV]|ll|LL|([iIuU])(8|16|32|64))?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[xX][0-9a-fA-F_]+([tTsSiIlLvV]|ll|LL|([iIuU])(8|16|32|64))?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "(0|[1-9][0-9_]*)([tTsSiIlLvV]|ll|LL|([iIuU])(8|16|32|64))?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tokNext "([rR][cC]?|[cC][rR])\"\"\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo tdqs')
    , tokNext "([rR][cC]?|[cC][rR])'''" (Arbitrary "Literal" :. Arbitrary "String") (GoTo tsqs')
    , tokNext "([rR][cC]?|[cC][rR])\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo dqs')
    , tokNext "([rR][cC]?|[cC][rR])'" (Arbitrary "Literal" :. Arbitrary "String") (GoTo sqs')
    , tokNext "[cCfFqQwWuU]?\"\"\"" (Arbitrary "Literal" :. Arbitrary "String") (Combined [stringescape', tdqs'])
    , tokNext "[cCfFqQwWuU]?'''" (Arbitrary "Literal" :. Arbitrary "String") (Combined [stringescape', tsqs'])
    , tokNext "[cCfFqQwWuU]?\"" (Arbitrary "Literal" :. Arbitrary "String") (Combined [stringescape', dqs'])
    , tokNext "[cCfFqQwWuU]?'" (Arbitrary "Literal" :. Arbitrary "String") (Combined [stringescape', sqs'])
    , tok "[\\[\\]{}:(),;?]" (Arbitrary "Punctuation")
    , tok "[a-zA-Z_]\\w*:>" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "(_svc|while)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(root|self|this)\\b" (Arbitrary "Name" :. Arbitrary "Builtin" :. Arbitrary "Pseudo")
    , tok "[a-zA-Z_]\\w*" (Arbitrary "Name")
    ]

strings' :: TokenMatcher
strings' =
    [ tok "%(\\([a-zA-Z0-9]+\\))?[-#0 +]*([0-9]+|[*])?(\\.([0-9]+|[*]))?[hlL]?[diouxXeEfFgGcrs%]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol")
    , tok "[^\\\\\\'\"%\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "[\\'\"\\\\]" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "%" (Arbitrary "Literal" :. Arbitrary "String")
    ]

tvarlist' :: TokenMatcher
tvarlist' =
    [ anyOf whitespace'
    , anyOf operators'
    , tokNext "\\[" (Arbitrary "Punctuation") Push
    , tokNext "\\]" (Arbitrary "Punctuation") Pop
    , tok "," (Arbitrary "Punctuation")
    , tok "(with|where)\\b" (Arbitrary "Keyword")
    , tok "[a-zA-Z_]\\w*" (Arbitrary "Name")
    ]

