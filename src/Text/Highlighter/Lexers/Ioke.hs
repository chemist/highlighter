module Text.Highlighter.Lexers.Ioke (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Ioke"
    , lAliases = ["ioke", "ik"]
    , lExtensions = [".ik"]
    , lMimetypes = ["text/x-iokesrc"]
    , lStart = root'
    , lFlags = [multiline]
    }

squareRegexp' :: TokenMatcher
squareRegexp' =
    [ tokNext "(?<!\\\\)][oxpniums]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , anyOf interpolatableText'
    , tok "\\\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "[^\\]]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    ]

textInterpolationRoot' :: TokenMatcher
textInterpolationRoot' =
    [ tokNext "}" (Arbitrary "Punctuation") Pop
    , anyOf root'
    ]

interpolatableText' :: TokenMatcher
interpolatableText' =
    [ tok "(\\\\b|\\\\e|\\\\t|\\\\n|\\\\f|\\\\r|\\\\\"|\\\\\\\\|\\\\#|\\\\\\Z|\\\\u[0-9a-fA-F]{1,4}|\\\\[0-3]?[0-7]?[0-7])" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tokNext "#{" (Arbitrary "Punctuation") (GoTo textInterpolationRoot')
    ]

text' :: TokenMatcher
text' =
    [ tokNext "(?<!\\\\)\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf interpolatableText'
    , tok "[^\"]" (Arbitrary "Literal" :. Arbitrary "String")
    ]

documentation' :: TokenMatcher
documentation' =
    [ tokNext "(?<!\\\\)\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Doc") Pop
    , anyOf interpolatableText'
    , tok "[^\"]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Doc")
    ]

slashRegexp' :: TokenMatcher
slashRegexp' =
    [ tokNext "(?<!\\\\)/[oxpniums]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , anyOf interpolatableText'
    , tok "\\\\/" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "[^/]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    ]

squareText' :: TokenMatcher
squareText' =
    [ tokNext "(?<!\\\\)]" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf interpolatableText'
    , tok "[^\\]]" (Arbitrary "Literal" :. Arbitrary "String")
    ]

root' :: TokenMatcher
root' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok ";(.*?)\\n" (Arbitrary "Comment")
    , tok "\\A#!(.*?)\\n" (Arbitrary "Comment")
    , tokNext "#/" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") (GoTo slashRegexp')
    , tokNext "#r\\[" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") (GoTo squareRegexp')
    , tok ":[a-zA-Z0-9_!:?]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "[a-zA-Z0-9_!:?]+:(?![a-zA-Z0-9_!?])" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tok ":\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tokNext "((?<=fn\\()|(?<=fnx\\()|(?<=method\\()|(?<=macro\\()|(?<=lecro\\()|(?<=syntax\\()|(?<=dmacro\\()|(?<=dlecro\\()|(?<=dlecrox\\()|(?<=dsyntax\\())[\\s\\n\\r]*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Doc") (GoTo documentation')
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo text')
    , tokNext "#\\[" (Arbitrary "Literal" :. Arbitrary "String") (GoTo squareText')
    , tok "[a-zA-Z0-9_][a-zA-Z0-9!?_:]+(?=\\s*=.*mimic\\s)" (Arbitrary "Name" :. Arbitrary "Entity")
    , tok "[a-zA-Z_][a-zA-Z0-9_!:?]*(?=[\\s]*[+*/-]?=[^=].*($|\\.))" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(break|cond|continue|do|ensure|for|for:dict|for:set|if|let|loop|p:for|p:for:dict|p:for:set|return|unless|until|while|with)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(eval|mimic|print|println)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword")
    , tok "(cell\\?|cellNames|cellOwner\\?|cellOwner|cells|cell|documentation|hash|identity|mimic|removeCell\\!|undefineCell\\!)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword")
    , tok "(stackTraceAsText)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword")
    , tok "(dict|list|message|set)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(case|case:and|case:else|case:nand|case:nor|case:not|case:or|case:otherwise|case:xor)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(asText|become\\!|derive|freeze\\!|frozen\\?|in\\?|is\\?|kind\\?|mimic\\!|mimics|mimics\\?|prependMimic\\!|removeAllMimics\\!|removeMimic\\!|same\\?|send|thaw\\!|uniqueHexId)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword")
    , tok "(after|around|before)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(kind|cellDescriptionDict|cellSummary|genSym|inspect|notice)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword")
    , tok "(use|destructuring)" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(cell\\?|cellOwner\\?|cellOwner|cellNames|cells|cell|documentation|identity|removeCell!|undefineCell)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword")
    , tok "(internal:compositeRegexp|internal:concatenateText|internal:createDecimal|internal:createNumber|internal:createRegexp|internal:createText)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(availableRestarts|bind|error\\!|findRestart|handle|invokeRestart|rescue|restart|signal\\!|warn\\!)(?![a-zA-Z0-9!:_?])" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(nil|false|true)(?![a-zA-Z0-9!:_?])" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "(Arity|Base|Call|Condition|DateTime|Aspects|Pointcut|Assignment|BaseBehavior|Boolean|Case|AndCombiner|Else|NAndCombiner|NOrCombiner|NotCombiner|OrCombiner|XOrCombiner|Conditions|Definitions|FlowControl|Internal|Literals|Reflection|DefaultMacro|DefaultMethod|DefaultSyntax|Dict|FileSystem|Ground|Handler|Hook|IO|IokeGround|Struct|LexicalBlock|LexicalMacro|List|Message|Method|Mixins|NativeMethod|Number|Origin|Pair|Range|Reflector|Regexp Match|Regexp|Rescue|Restart|Runtime|Sequence|Set|Symbol|System|Text|Tuple)(?![a-zA-Z0-9!:_?])" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(generateMatchMethod|aliasMethod|\955|\654|fnx|fn|method|dmacro|dlecro|syntax|macro|dlecrox|lecrox|lecro|syntax)(?![a-zA-Z0-9!:_?])" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "-?0[xX][0-9a-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "-?(\\d+\\.?\\d*|\\d*\\.\\d+)([eE][+-]?[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "-?\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "#\\(" (Arbitrary "Punctuation")
    , tok "(&&>>|\\|\\|>>|\\*\\*>>|:::|::|\\.\\.\\.|===|\\*\\*>|\\*\\*=|&&>|&&=|\\|\\|>|\\|\\|=|\\->>|\\+>>|!>>|<>>>|<>>|&>>|%>>|#>>|@>>|/>>|\\*>>|\\?>>|\\|>>|\\^>>|\126>>|\\$>>|=>>|<<=|>>=|<=>|<\\->|=\126|!\126|=>|\\+\\+|\\-\\-|<=|>=|==|!=|&&|\\.\\.|\\+=|\\-=|\\*=|\\/=|%=|&=|\\^=|\\|=|<\\-|\\+>|!>|<>|&>|%>|#>|\\@>|\\/>|\\*>|\\?>|\\|>|\\^>|\126>|\\$>|<\\->|\\->|<<|>>|\\*\\*|\\?\\||\\?&|\\|\\||>|<|\\*|\\/|%|\\+|\\-|&|\\^|\\||=|\\$|!|\126|\\?|#|\8800|\8728|\8712|\8713)" (Arbitrary "Operator")
    , tok "(and|nand|or|xor|nor|return|import)(?![a-zA-Z0-9_!?])" (Arbitrary "Operator")
    , tok "(\\`\\`|\\`|\\'\\'|\\'|\\.|\\,|@|@@|\\[|\\]|\\(|\\)|{|})" (Arbitrary "Punctuation")
    , tok "[A-Z][a-zA-Z0-9_!:?]*" (Arbitrary "Name" :. Arbitrary "Class")
    , tok "[a-z_][a-zA-Z0-9_!:?]*" (Arbitrary "Name")
    ]

