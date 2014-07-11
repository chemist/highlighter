module Text.Highlighter.Lexers.Boo (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Boo"
    , lAliases = ["boo"]
    , lExtensions = [".boo"]
    , lMimetypes = ["text/x-boo"]
    , lStart = root'
    , lFlags = [multiline]
    }

comment' :: TokenMatcher
comment' =
    [ tokNext "/[*]" (Arbitrary "Comment" :. Arbitrary "Multiline") Push
    , tokNext "[*]/" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tok "[^/*]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "[*/]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

classname' :: TokenMatcher
classname' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

namespace' :: TokenMatcher
namespace' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_.]*" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "(#|//).*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "/[*]" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo comment')
    , tok "[]{}:(),.;[]" (Arbitrary "Punctuation")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "\\\\" (Arbitrary "Text")
    , tok "(in|is|and|or|not)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "/(\\\\\\\\|\\\\/|[^/\\s])/" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "@/(\\\\\\\\|\\\\/|[^/])*/" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "=\126|!=|==|<<|>>|[-+/*%=<>&^|]" (Arbitrary "Operator")
    , tok "(as|abstract|callable|constructor|destructor|do|import|enum|event|final|get|interface|internal|of|override|partial|private|protected|public|return|set|static|struct|transient|virtual|yield|super|and|break|cast|continue|elif|else|ensure|except|for|given|goto|if|in|is|isa|not|or|otherwise|pass|raise|ref|try|unless|when|while|from|as)\\b" (Arbitrary "Keyword")
    , tok "def(?=\\s+\\(.*?\\))" (Arbitrary "Keyword")
    , tokNext "(def)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo funcname')
    , tokNext "(class)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tokNext "(namespace)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo namespace')
    , tok "(?<!\\.)(true|false|null|self|__eval__|__switch__|array|assert|checked|enumerate|filter|getter|len|lock|map|matrix|max|min|normalArrayIndexing|print|property|range|rawArrayIndexing|required|typeof|unchecked|using|yieldAll|zip)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "\"\"\"(\\\\|\\\"|.*?)\"\"\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "\"(\\\\|\\\"|[^\"]*?)\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(\\\\|\\'|[^']*?)'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    , tok "(\\d+\\.\\d*|\\d*\\.\\d+)([fF][+-]?[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9][0-9\\.]*(m|ms|d|h|s)" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0x[a-fA-F0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "\\d+L" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer" :. Arbitrary "Long")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    ]

funcname' :: TokenMatcher
funcname' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Function") Pop
    ]

