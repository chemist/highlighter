module Text.Highlighter.Lexers.Prolog (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Prolog"
    , lAliases = ["prolog"]
    , lExtensions = [".prolog", ".pro", ".pl"]
    , lMimetypes = ["text/x-prolog"]
    , lStart = root'
    , lFlags = [utf8]
    }

nestedComment' :: TokenMatcher
nestedComment' =
    [ tokNext "\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tokNext "/\\*" (Arbitrary "Comment" :. Arbitrary "Multiline") Push
    , tok "[^*/]+" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "[*/]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

root' :: TokenMatcher
root' =
    [ tok "^#.*" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "/\\*" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo nestedComment')
    , tok "%.*" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[\\[\\](){}|.,;!]" (Arbitrary "Punctuation")
    , tok ":-|-->" (Arbitrary "Punctuation")
    , tok "\"(?:\\\\x[0-9a-fA-F]+\\\\|\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8}|\\\\[0-7]+\\\\|\\\\[\\w\\W]|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(?:''|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Atom")
    , tok "(is|<|>|=<|>=|==|=:=|=|/|//|\\*|\\+|-)(?=\\s|[a-zA-Z0-9\\[])" (Arbitrary "Operator")
    , tok "(mod|div|not)\\b" (Arbitrary "Operator")
    , tok "_" (Arbitrary "Keyword")
    , tok "([a-z]+)(:)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Namespace"), (Arbitrary "Punctuation")])
    , tok "([a-z\192-\8191\12352-\55295\57344-\65519][a-zA-Z0-9_$\192-\8191\12352-\55295\57344-\65519]*)(\\s*)(:-|-->)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tok "([a-z\192-\8191\12352-\55295\57344-\65519][a-zA-Z0-9_$\192-\8191\12352-\55295\57344-\65519]*)(\\s*)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Punctuation")])
    , tok "[a-z\192-\8191\12352-\55295\57344-\65519][a-zA-Z0-9_$\192-\8191\12352-\55295\57344-\65519]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Atom")
    , tok "[#&*+\\-./:<=>?@\\\\^\126\161-\191\8208-\12351]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Atom")
    , tok "[A-Z_][A-Za-z0-9_]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "\\s+|[\8192-\8207\65520-\65534\65519]" (Arbitrary "Text")
    ]

