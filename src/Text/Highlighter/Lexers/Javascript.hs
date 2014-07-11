module Text.Highlighter.Lexers.Javascript (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "JavaScript"
    , lAliases = ["js", "javascript"]
    , lExtensions = [".js"]
    , lMimetypes = ["application/javascript", "application/x-javascript", "text/x-javascript", "text/javascript"]
    , lStart = root'
    , lFlags = [dotall]
    }

commentsandwhitespace' :: TokenMatcher
commentsandwhitespace' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "<!--" (Arbitrary "Comment")
    , tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

root' :: TokenMatcher
root' =
    [ tokNext "^(?=\\s|/|<!--)" (Arbitrary "Text") (GoTo slashstartsregex')
    , anyOf commentsandwhitespace'
    , tokNext "\\+\\+|--|\126|&&|\\?|:|\\|\\||\\\\(?=\\n)|(<<|>>>?|==?|!=?|[-<>+*%&\\|\\^/])=?" (Arbitrary "Operator") (GoTo slashstartsregex')
    , tokNext "[{(\\[;,]" (Arbitrary "Punctuation") (GoTo slashstartsregex')
    , tok "[})\\].]" (Arbitrary "Punctuation")
    , tokNext "(for|in|while|do|break|return|continue|switch|case|default|if|else|throw|try|catch|finally|new|delete|typeof|instanceof|void|this)\\b" (Arbitrary "Keyword") (GoTo slashstartsregex')
    , tokNext "(var|with|function)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration") (GoTo slashstartsregex')
    , tok "(abstract|boolean|byte|char|class|const|debugger|double|enum|export|extends|final|float|goto|implements|import|int|interface|long|native|package|private|protected|public|short|static|super|synchronized|throws|transient|volatile)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(true|false|null|NaN|Infinity|undefined)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(Array|Boolean|Date|Error|Function|Math|netscape|Number|Object|Packages|RegExp|String|sun|decodeURI|decodeURIComponent|encodeURI|encodeURIComponent|Error|eval|isFinite|isNaN|parseFloat|parseInt|document|this|window)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[$a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Other")
    , tok "[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    ]

slashstartsregex' :: TokenMatcher
slashstartsregex' =
    [ anyOf commentsandwhitespace'
    , tokNext "/(\\\\.|[^[/\\\\\\n]|\\[(\\\\.|[^\\]\\\\\\n])*])+/([gim]+\\b|\\B)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "(?=/)" (Arbitrary "Text") (DoAll [Pop, (GoTo badregex')])
    , tokNext "" (Arbitrary "Text") Pop
    ]

badregex' :: TokenMatcher
badregex' =
    [ tokNext "\10" (Arbitrary "Text") Pop
    ]

