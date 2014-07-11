module Text.Highlighter.Lexers.Nasm (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "NASM"
    , lAliases = ["nasm"]
    , lExtensions = [".asm", ".ASM"]
    , lMimetypes = ["text/x-nasm"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

preproc' :: TokenMatcher
preproc' =
    [ tok "[^;\\n]+" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext ";.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single") Pop
    , tokNext "\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    ]

punctuation' :: TokenMatcher
punctuation' =
    [ tok "[,():\\[\\]]+" (Arbitrary "Punctuation")
    , tok "[&|^<>+*/%\126-]+" (Arbitrary "Operator")
    , tok "[$]+" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "seg|wrt|strict" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "byte|[dq]?word" (Arbitrary "Keyword" :. Arbitrary "Type")
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tokNext "^\\s*%" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo preproc')
    , tok "[a-zA-Z$._?][a-zA-Z0-9$._?#@\126]*:" (Arbitrary "Name" :. Arbitrary "Label")
    , tokNext "([a-zA-Z$._?][a-zA-Z0-9$._?#@\126]*)(\\s+)(equ)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Constant"), (Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Keyword" :. Arbitrary "Declaration")]) (GoTo instructionArgs')
    , tokNext "BITS|USE16|USE32|SECTION|SEGMENT|ABSOLUTE|EXTERN|GLOBAL|ORG|ALIGN|STRUC|ENDSTRUC|COMMON|CPU|GROUP|UPPERCASE|IMPORT|EXPORT|LIBRARY|MODULE" (Arbitrary "Keyword") (GoTo instructionArgs')
    , tokNext "(?:res|d)[bwdqt]|times" (Arbitrary "Keyword" :. Arbitrary "Declaration") (GoTo instructionArgs')
    , tokNext "[a-zA-Z$._?][a-zA-Z0-9$._?#@\126]*" (Arbitrary "Name" :. Arbitrary "Function") (GoTo instructionArgs')
    , tok "[\\r\\n]+" (Arbitrary "Text")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "[ \\t]+" (Arbitrary "Text")
    , tok ";.*" (Arbitrary "Comment" :. Arbitrary "Single")
    ]

instructionArgs' :: TokenMatcher
instructionArgs' =
    [ tok "\"(\\\\\"|[^\"])*\"|'(\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "(?:0[xX][0-9a-fA-F]+|$0[0-9a-fA-F]*|[0-9]+[0-9a-fA-F]*h)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-7]+q" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "[01]+b" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[0-9]+\\.e?[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , anyOf punctuation'
    , tok "[a-d][lh]|e?[a-d]x|e?[sb]p|e?[sd]i|[c-gs]s|st[0-7]|mm[0-7]|cr[0-4]|dr[0-367]|tr[3-7]" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[a-zA-Z$._?][a-zA-Z0-9$._?#@\126]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tokNext "[\\r\\n]+" (Arbitrary "Text") Pop
    , anyOf whitespace'
    ]

