module Text.Highlighter.Lexers.D (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "D"
    , lAliases = ["d"]
    , lExtensions = [".d", ".di"]
    , lMimetypes = ["text/x-dsrc"]
    , lStart = root'
    , lFlags = [multiline]
    }

delimited_parenthesis' :: TokenMatcher
delimited_parenthesis' =
    [ tok "[^\\(\\)]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\\(" (Arbitrary "Literal" :. Arbitrary "String") (GoTo delimited_inside_parenthesis')
    , tokNext "\\)\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

delimited_curly' :: TokenMatcher
delimited_curly' =
    [ tok "[^{}]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "{" (Arbitrary "Literal" :. Arbitrary "String") (GoTo delimited_inside_curly')
    , tokNext "}\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

delimited_angle' :: TokenMatcher
delimited_angle' =
    [ tok "[^<>]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "<" (Arbitrary "Literal" :. Arbitrary "String") (GoTo delimited_inside_angle')
    , tokNext ">\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

delimited_inside_parenthesis' :: TokenMatcher
delimited_inside_parenthesis' =
    [ tok "[^\\(\\)]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\\(" (Arbitrary "Literal" :. Arbitrary "String") Push
    , tokNext "\\)" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

delimited_inside_curly' :: TokenMatcher
delimited_inside_curly' =
    [ tok "[^{}]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "{" (Arbitrary "Literal" :. Arbitrary "String") Push
    , tokNext "}" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

delimited_inside_bracket' :: TokenMatcher
delimited_inside_bracket' =
    [ tok "[^\\[\\]]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\\[" (Arbitrary "Literal" :. Arbitrary "String") Push
    , tokNext "\\]" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

token_string_nest' :: TokenMatcher
token_string_nest' =
    [ tokNext "{" (Arbitrary "Punctuation") Push
    , tokNext "}" (Arbitrary "Punctuation") Pop
    , anyOf root'
    ]

delimited_bracket' :: TokenMatcher
delimited_bracket' =
    [ tok "[^\\[\\]]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\\[" (Arbitrary "Literal" :. Arbitrary "String") (GoTo delimited_inside_bracket')
    , tokNext "\\]\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

token_string' :: TokenMatcher
token_string' =
    [ tokNext "{" (Arbitrary "Punctuation") (GoTo token_string_nest')
    , tokNext "}" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf root'
    ]

delimited_inside_angle' :: TokenMatcher
delimited_inside_angle' =
    [ tok "[^<>]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "<" (Arbitrary "Literal" :. Arbitrary "String") Push
    , tokNext ">" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "//(.*?)\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "/\\+" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo nested_comment')
    , tok "(abstract|alias|align|asm|assert|auto|body|break|case|cast|catch|class|const|continue|debug|default|delegate|delete|deprecated|do|else|enum|export|extern|finally|final|foreach_reverse|foreach|for|function|goto|if|import|inout|interface|invariant|in|is|lazy|mixin|module|new|nothrow|out|override|package|pragma|private|protected|public|pure|ref|return|scope|static|struct|super|switch|synchronized|template|this|throw|try|typedef|typeid|typeof|union|unittest|version|volatile|while|with|__traits)\\b" (Arbitrary "Keyword")
    , tok "(bool|byte|cdouble|cent|cfloat|char|creal|dchar|double|float|idouble|ifloat|int|ireal|long|real|short|ubyte|ucent|uint|ulong|ushort|void|wchar)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(false|true|null)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "macro\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(string|wstring|dstring)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "0[xX]([0-9a-fA-F_]*\\.[0-9a-fA-F_]+|[0-9a-fA-F_]+)[pP][+\\-]?[0-9_]+[fFL]?[i]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9_]+(\\.[0-9_]+[eE][+\\-]?[0-9_]+|\\.[0-9_]*|[eE][+\\-]?[0-9_]+)[fFL]?[i]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\.(0|[1-9][0-9_]*)([eE][+\\-]?[0-9_]+)?[fFL]?[i]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[Bb][01_]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0[0-7_]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[xX][0-9a-fA-F_]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "(0|[1-9][0-9_]*)([LUu]|Lu|LU|uL|UL)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "'(\\\\['\"?\\\\abfnrtv]|\\\\x[0-9a-fA-F]{2}|\\\\[0-7]{1,3}|\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8}|\\\\&\\w+;|.)'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "r\"[^\"]*\"[cwd]?" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "`[^`]*`[cwd]?" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"[cwd]?" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\(['\\\"?\\\\abfnrtv]|x[0-9a-fA-F]{2}|[0-7]{1,3}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}|&\\w+;)" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "x\"[0-9a-fA-F_\\s]*\"[cwd]?" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "q\"\\[" (Arbitrary "Literal" :. Arbitrary "String") (GoTo delimited_bracket')
    , tokNext "q\"\\(" (Arbitrary "Literal" :. Arbitrary "String") (GoTo delimited_parenthesis')
    , tokNext "q\"<" (Arbitrary "Literal" :. Arbitrary "String") (GoTo delimited_angle')
    , tokNext "q\"{" (Arbitrary "Literal" :. Arbitrary "String") (GoTo delimited_curly')
    , tok "q\"([a-zA-Z_]\\w*)\\n.*?\\n\\1\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "q\"(.).*?\\1\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "q{" (Arbitrary "Literal" :. Arbitrary "String") (GoTo token_string')
    , tok "(\126=|\\^=|%=|\\*=|==|!>=|!<=|!<>=|!<>|!<|!>|!=|>>>=|>>>|>>=|>>|>=|<>=|<>|<<=|<<|<=|\\+\\+|\\+=|--|-=|\\|\\||\\|=|&&|&=|\\.\\.\\.|\\.\\.|/=)|[/.&|\\-+<>!()\\[\\]{}?,;:$=*%^\126]" (Arbitrary "Punctuation")
    , tok "[a-zA-Z_]\\w*" (Arbitrary "Name")
    ]

nested_comment' :: TokenMatcher
nested_comment' =
    [ tok "[^+/]+" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "/\\+" (Arbitrary "Comment" :. Arbitrary "Multiline") Push
    , tokNext "\\+/" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tok "[+/]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

