module Text.Highlighter.Lexers.ActionScript3 (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "ActionScript 3"
    , lAliases = ["as3", "actionscript3"]
    , lExtensions = [".as"]
    , lMimetypes = ["application/x-actionscript", "text/x-actionscript", "text/actionscript"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

defval' :: TokenMatcher
defval' =
    [ tokNext "(=)(\\s*)([^(),]+)(\\s*)(,?)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Text"), (Using lexer), (Arbitrary "Text"), (Arbitrary "Operator")]) Pop
    , tokNext ",?" (Arbitrary "Operator") Pop
    ]

type' :: TokenMatcher
type' =
    [ tokNext "(\\s*)(:)(\\s*)([$a-zA-Z_][a-zA-Z0-9_]*|\\*)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type")]) (PopNum 2)
    , tokNext "\\s*" (Arbitrary "Text") (PopNum 2)
    ]

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "(function\\s+)([$a-zA-Z_][a-zA-Z0-9_]*)(\\s*)(\\()" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Operator")]) (GoTo funcparams')
    , tok "(var|const)(\\s+)([$a-zA-Z_][a-zA-Z0-9_]*)(\\s*)(:)(\\s*)([$a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type")])
    , tok "(import|package)(\\s+)((?:[$a-zA-Z_][a-zA-Z0-9_]*|\\.)+)(\\s*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Namespace"), (Arbitrary "Text")])
    , tok "(new)(\\s+)([$a-zA-Z_][a-zA-Z0-9_]*)(\\s*)(\\()" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "/(\\\\\\\\|\\\\/|[^\\n])*/[gisx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "(\\.)([$a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Name" :. Arbitrary "Attribute")])
    , tok "(case|default|for|each|in|while|do|break|return|continue|if|else|throw|try|catch|with|new|typeof|arguments|instanceof|this|switch|import|include|as|is)\\b" (Arbitrary "Keyword")
    , tok "(class|public|final|internal|native|override|private|protected|static|import|extends|implements|interface|intrinsic|return|super|dynamic|function|const|get|namespace|package|set)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "(true|false|null|NaN|Infinity|-Infinity|undefined|void)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(decodeURI|decodeURIComponent|encodeURI|escape|eval|isFinite|isNaN|isXMLName|clearInterval|fscommand|getTimer|getURL|getVersion|isFinite|parseFloat|parseInt|setInterval|trace|updateAfterEvent|unescape)\\b" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "[$a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    , tok "[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-f]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "[\126\\^\\*!%&<>\\|+=:;,/?\\\\{}\\[\\]();.-]+" (Arbitrary "Operator")
    ]

funcparams' :: TokenMatcher
funcparams' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "(\\s*)(\\.\\.\\.)?([$a-zA-Z_][a-zA-Z0-9_]*)(\\s*)(:)(\\s*)([$a-zA-Z_][a-zA-Z0-9_]*|\\*)(\\s*)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name"), (Arbitrary "Text"), (Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Text")]) (GoTo defval')
    , tokNext "\\)" (Arbitrary "Operator") (GoTo type')
    ]

