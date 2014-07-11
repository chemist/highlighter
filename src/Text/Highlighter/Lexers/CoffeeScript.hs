module Text.Highlighter.Lexers.CoffeeScript (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "CoffeeScript"
    , lAliases = ["coffee-script", "coffeescript"]
    , lExtensions = [".coffee"]
    , lMimetypes = ["text/coffeescript"]
    , lStart = root'
    , lFlags = [dotall]
    }

commentsandwhitespace' :: TokenMatcher
commentsandwhitespace' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "#.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    ]

root' :: TokenMatcher
root' =
    [ tokNext "^(?=\\s|/|<!--)" (Arbitrary "Text") (GoTo slashstartsregex')
    , anyOf commentsandwhitespace'
    , tokNext "\\+\\+|--|\126|&&|\\band\\b|\\bor\\b|\\bis\\b|\\bisnt\\b|\\bnot\\b|\\?|:|=|\\|\\||\\\\(?=\\n)|(<<|>>>?|==?|!=?|[-<>+*`%&\\|\\^/])=?" (Arbitrary "Operator") (GoTo slashstartsregex')
    , tok "\\([^()]*\\)\\s*->" (Arbitrary "Name" :. Arbitrary "Function")
    , tokNext "[{(\\[;,]" (Arbitrary "Punctuation") (GoTo slashstartsregex')
    , tok "[})\\].]" (Arbitrary "Punctuation")
    , tokNext "(for|in|of|while|break|return|continue|switch|when|then|if|else|throw|try|catch|finally|new|delete|typeof|instanceof|super|extends|this|class|by)\\b" (Arbitrary "Keyword") (GoTo slashstartsregex')
    , tok "(true|false|yes|no|on|off|null|NaN|Infinity|undefined)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(Array|Boolean|Date|Error|Function|Math|netscape|Number|Object|Packages|RegExp|String|sun|decodeURI|decodeURIComponent|encodeURI|encodeURIComponent|eval|isFinite|isNaN|parseFloat|parseInt|document|window)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tokNext "[$a-zA-Z_][a-zA-Z0-9_\\.:]*\\s*[:=]\\s" (Arbitrary "Name" :. Arbitrary "Variable") (GoTo slashstartsregex')
    , tokNext "@[$a-zA-Z_][a-zA-Z0-9_\\.:]*\\s*[:=]\\s" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Instance") (GoTo slashstartsregex')
    , tokNext "@?[$a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Other") (GoTo slashstartsregex')
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

