module Text.Highlighter.Lexers.ObjectiveJ (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Objective-J"
    , lAliases = ["objective-j", "objectivej", "obj-j", "objj"]
    , lExtensions = [".j"]
    , lMimetypes = ["text/x-objective-j"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

statements' :: TokenMatcher
statements' =
    [ tokNext "(L|@)?\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "(L|@)?'(\\\\.|\\\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\'\\n])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+)[eE][+-]?\\d+[lL]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-fA-F]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\d+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tokNext "^(?=\\s|/|<!--)" (Arbitrary "Text") (GoTo slashstartsregex')
    , tokNext "\\+\\+|--|\126|&&|\\?|:|\\|\\||\\\\(?=\\n)|(<<|>>>?|==?|!=?|[-<>+*%&\\|\\^/])=?" (Arbitrary "Operator") (GoTo slashstartsregex')
    , tokNext "[{(\\[;,]" (Arbitrary "Punctuation") (GoTo slashstartsregex')
    , tok "[})\\].]" (Arbitrary "Punctuation")
    , tokNext "(for|in|while|do|break|return|continue|switch|case|default|if|else|throw|try|catch|finally|new|delete|typeof|instanceof|void|prototype|__proto__)\\b" (Arbitrary "Keyword") (GoTo slashstartsregex')
    , tokNext "(var|with|function)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration") (GoTo slashstartsregex')
    , tok "(@selector|@private|@protected|@public|@encode|@synchronized|@try|@throw|@catch|@finally|@end|@property|@synthesize|@dynamic|@for|@accessors|new)\\b" (Arbitrary "Keyword")
    , tok "(int|long|float|short|double|char|unsigned|signed|void|id|BOOL|bool|boolean|IBOutlet|IBAction|SEL|@outlet|@action)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(self|super)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(TRUE|YES|FALSE|NO|Nil|nil|NULL)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(true|false|null|NaN|Infinity|undefined)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(ABS|ASIN|ACOS|ATAN|ATAN2|SIN|COS|TAN|EXP|POW|CEIL|FLOOR|ROUND|MIN|MAX|RAND|SQRT|E|LN2|LN10|LOG2E|LOG10E|PI|PI2|PI_2|SQRT1_2|SQRT2)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(Array|Boolean|Date|Error|Function|Math|netscape|Number|Object|Packages|RegExp|String|sun|decodeURI|decodeURIComponent|encodeURI|encodeURIComponent|Error|eval|isFinite|isNaN|parseFloat|parseInt|document|this|window)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "([$a-zA-Z_][a-zA-Z0-9_]*)((?:\\s|//.*?\\n|/[*].*?[*]/)*)(?=\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Using lexer)])
    , tok "[$a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

function_signature' :: TokenMatcher
function_signature' =
    [ anyOf whitespace'
    , tokNext "(\\((?:\\s|//.*?\\n|/[*].*?[*]/)*)([a-zA-Z_][a-zA-Z0-9_]+)((?:\\s|//.*?\\n|/[*].*?[*]/)*\\)(?:\\s|//.*?\\n|/[*].*?[*]/)*)([$a-zA-Z_][a-zA-Z0-9_]+(?:\\s|//.*?\\n|/[*].*?[*]/)*:)" (ByGroups [(Using lexer), (Arbitrary "Keyword" :. Arbitrary "Type"), (Using lexer), (Arbitrary "Name" :. Arbitrary "Function")]) (GoTo function_parameters')
    , tokNext "(\\((?:\\s|//.*?\\n|/[*].*?[*]/)*)([a-zA-Z_][a-zA-Z0-9_]+)((?:\\s|//.*?\\n|/[*].*?[*]/)*\\)(?:\\s|//.*?\\n|/[*].*?[*]/)*)([$a-zA-Z_][a-zA-Z0-9_]+)" (ByGroups [(Using lexer), (Arbitrary "Keyword" :. Arbitrary "Type"), (Using lexer), (Arbitrary "Name" :. Arbitrary "Function")]) Pop
    , tokNext "([$a-zA-Z_][a-zA-Z0-9_]+(?:\\s|//.*?\\n|/[*].*?[*]/)*:)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function")]) (GoTo function_parameters')
    , tokNext "([$a-zA-Z_][a-zA-Z0-9_]+)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function")]) Pop
    , tokNext "" (Arbitrary "Text") Pop
    ]

classname' :: TokenMatcher
classname' =
    [ tokNext "([a-zA-Z_][a-zA-Z0-9_]*)((?:\\s|//.*?\\n|/[*].*?[*]/)*:(?:\\s|//.*?\\n|/[*].*?[*]/)*)([a-zA-Z_][a-zA-Z0-9_]*)?" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Using lexer), (Arbitrary "Name" :. Arbitrary "Class")]) Pop
    , tokNext "([a-zA-Z_][a-zA-Z0-9_]*)((?:\\s|//.*?\\n|/[*].*?[*]/)*\\()([a-zA-Z_][a-zA-Z0-9_]*)(\\))" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Using lexer), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text")]) Pop
    , tokNext "([a-zA-Z_][a-zA-Z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

forward_classname' :: TokenMatcher
forward_classname' =
    [ tokNext "([a-zA-Z_][a-zA-Z0-9_]*)(\\s*,\\s*)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text")]) Push
    , tokNext "([a-zA-Z_][a-zA-Z0-9_]*)(\\s*;?)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text")]) Pop
    ]

badregex' :: TokenMatcher
badregex' =
    [ tokNext "\10" (Arbitrary "Text") Pop
    ]

function_parameters' :: TokenMatcher
function_parameters' =
    [ anyOf whitespace'
    , tok "(\\((?:\\s|//.*?\\n|/[*].*?[*]/)*)([^\\)]+)((?:\\s|//.*?\\n|/[*].*?[*]/)*\\)(?:\\s|//.*?\\n|/[*].*?[*]/)*)+([$a-zA-Z_][a-zA-Z0-9_]+)" (ByGroups [(Using lexer), (Arbitrary "Keyword" :. Arbitrary "Type"), (Using lexer), (Arbitrary "Text")])
    , tok "([$a-zA-Z_][a-zA-Z0-9_]+(?:\\s|//.*?\\n|/[*].*?[*]/)*:)" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "(:)" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "(,(?:\\s|//.*?\\n|/[*].*?[*]/)*...)" (Using lexer)
    , tok "([$a-zA-Z_][a-zA-Z0-9_]+)" (Arbitrary "Text")
    ]

if0' :: TokenMatcher
if0' =
    [ tokNext "^\\s*#if.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Push
    , tokNext "^\\s*#endif.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok ".*?\\n" (Arbitrary "Comment")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "(@import)(\\s+)(\"(\\\\\\\\|\\\\\"|[^\"])*\")" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")])
    , tok "(@import)(\\s+)(<(\\\\\\\\|\\\\>|[^>])*>)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")])
    , tok "(#(?:include|import))(\\s+)(\"(\\\\\\\\|\\\\\"|[^\"])*\")" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")])
    , tok "(#(?:include|import))(\\s+)(<(\\\\\\\\|\\\\>|[^>])*>)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")])
    , tokNext "#if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tokNext "#" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    , tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "//(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "<!--" (Arbitrary "Comment")
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tok "^((?:\\s|//.*?\\n|/[*].*?[*]/)*[\\+-](?:\\s|//.*?\\n|/[*].*?[*]/)*)([\\(a-zA-Z_].*?[^\\(])((?:\\s|//.*?\\n|/[*].*?[*]/)*{)" (ByGroups [(Using lexer), (Using lexer), (Using lexer)])
    , tokNext "(@interface|@implementation)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tokNext "(@class|@protocol)(\\s*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo forward_classname')
    , tok "(\\s*)(@end)(\\s*)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text")])
    , anyOf statements'
    , tok "[{\\(\\)}]" (Arbitrary "Punctuation")
    , tok ";" (Arbitrary "Punctuation")
    ]

slashstartsregex' :: TokenMatcher
slashstartsregex' =
    [ anyOf whitespace'
    , tokNext "/(\\\\.|[^[/\\\\\\n]|\\[(\\\\.|[^\\]\\\\\\n])*])+/([gim]+\\b|\\B)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "(?=/)" (Arbitrary "Text") (DoAll [Pop, (GoTo badregex')])
    , tokNext "" (Arbitrary "Text") Pop
    ]

macro' :: TokenMatcher
macro' =
    [ tok "[^/\\n]+" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "/[*](.|\\n)*?[*]/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single") Pop
    , tok "/" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "(?<=\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext "\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    ]

expression' :: TokenMatcher
expression' =
    [ tok "([$a-zA-Z_][a-zA-Z0-9_]*)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Punctuation")])
    , tokNext "(\\))" (Arbitrary "Punctuation") Pop
    ]

