module Text.Highlighter.Lexers.Java (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Java"
    , lAliases = ["java"]
    , lExtensions = [".java"]
    , lMimetypes = ["text/x-java"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

import' :: TokenMatcher
import' =
    [ tokNext "[a-zA-Z0-9_.]+\\*?" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "^(\\s*(?:[a-zA-Z_][a-zA-Z0-9_\\.\\[\\]]*\\s+)+?)([a-zA-Z_][a-zA-Z0-9_]*)(\\s*)(\\()" (ByGroups [(Using lexer), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tok "[^\\S\\n]+" (Arbitrary "Text")
    , tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "@[a-zA-Z_][a-zA-Z0-9_\\.]*" (Arbitrary "Name" :. Arbitrary "Decorator")
    , tok "(assert|break|case|catch|continue|default|do|else|finally|for|if|goto|instanceof|new|return|switch|this|throw|try|while)\\b" (Arbitrary "Keyword")
    , tok "(abstract|const|enum|extends|final|implements|native|private|protected|public|static|strictfp|super|synchronized|throws|transient|volatile)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "(boolean|byte|char|double|float|int|long|short|void)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(package)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")])
    , tok "(true|false|null)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tokNext "(class|interface)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text")]) (GoTo class')
    , tokNext "(import)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")]) (GoTo import')
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "'\\\\.'|'[^\\\\]'|'\\\\u[0-9a-f]{4}'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(\\.)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Name" :. Arbitrary "Attribute")])
    , tok "[a-zA-Z_][a-zA-Z0-9_]*:" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "[a-zA-Z_\\$][a-zA-Z0-9_]*" (Arbitrary "Name")
    , tok "[\126\\^\\*!%&\\[\\]\\(\\)\\{\\}<>\\|+=:;,./?-]" (Arbitrary "Operator")
    , tok "[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-f]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9]+L?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\n" (Arbitrary "Text")
    ]

class' :: TokenMatcher
class' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

