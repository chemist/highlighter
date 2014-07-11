module Text.Highlighter.Lexers.ObjectiveC (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Objective-C"
    , lAliases = ["objective-c", "objectivec", "obj-c", "objc"]
    , lExtensions = [".m"]
    , lMimetypes = ["text/x-objective-c"]
    , lStart = root'
    , lFlags = [multiline]
    }

classname' :: TokenMatcher
classname' =
    [ tokNext "([a-zA-Z$_][a-zA-Z0-9$_]*)(\\s*:\\s*)([a-zA-Z$_][a-zA-Z0-9$_]*)?" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Class")]) Pop
    , tokNext "([a-zA-Z$_][a-zA-Z0-9$_]*)(\\s*)(\\([a-zA-Z$_][a-zA-Z0-9$_]*\\))" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label")]) Pop
    , tokNext "([a-zA-Z$_][a-zA-Z0-9$_]*)" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

function' :: TokenMatcher
function' =
    [ anyOf whitespace'
    , anyOf statements'
    , tok ";" (Arbitrary "Punctuation")
    , tokNext "{" (Arbitrary "Punctuation") Push
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

statement' :: TokenMatcher
statement' =
    [ anyOf whitespace'
    , anyOf statements'
    , tok "[{}]" (Arbitrary "Punctuation")
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

statements' :: TokenMatcher
statements' =
    [ tokNext "(L|@)?\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "(L|@)?'(\\\\.|\\\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\'\\n])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+)[eE][+-]?\\d+[lL]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-fA-F]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\d+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[\126!%^&*+=|?:<>/-]" (Arbitrary "Operator")
    , tok "[()\\[\\],.]" (Arbitrary "Punctuation")
    , tok "(auto|break|case|const|continue|default|do|else|enum|extern|for|goto|if|register|restricted|return|sizeof|static|struct|switch|typedef|union|volatile|virtual|while|in|@selector|@private|@protected|@public|@encode|@synchronized|@try|@throw|@catch|@finally|@end|@property|@synthesize|@dynamic)\\b" (Arbitrary "Keyword")
    , tok "(int|long|float|short|double|char|unsigned|signed|void|id|BOOL|IBOutlet|IBAction|SEL)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(_{0,2}inline|naked|restrict|thread|typename)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|declspec|finally|int64|try|leave)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(TRUE|FALSE|nil|NULL)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[a-zA-Z$_][a-zA-Z0-9$_]*:(?!:)" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "[a-zA-Z$_][a-zA-Z0-9$_]*" (Arbitrary "Name")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tokNext "^#if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tokNext "^#" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    , tokNext "^(?:\\s|//.*?\\n|/[*].*?[*]/)+#if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tokNext "^(?:\\s|//.*?\\n|/[*].*?[*]/)+#" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    , tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "//(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

forward_classname' :: TokenMatcher
forward_classname' =
    [ tokNext "([a-zA-Z$_][a-zA-Z0-9$_]*)(\\s*,\\s*)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text")]) (GoTo forward_classname')
    , tokNext "([a-zA-Z$_][a-zA-Z0-9$_]*)(\\s*;?)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text")]) Pop
    ]

if0' :: TokenMatcher
if0' =
    [ tokNext "^\\s*#if.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Push
    , tokNext "^\\s*#endif.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok ".*?\\n" (Arbitrary "Comment")
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

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tokNext "((?:[a-zA-Z0-9_*\\s])+?(?:\\s|[*]))([a-zA-Z$_][a-zA-Z0-9$_]*)(\\s*\\([^;]*?\\))((?:\\s|//.*?\\n|/[*].*?[*]/)+)({)" (ByGroups [(Using lexer), (Arbitrary "Name" :. Arbitrary "Function"), (Using lexer), (Arbitrary "Text"), (Arbitrary "Punctuation")]) (GoTo function')
    , tok "((?:[a-zA-Z0-9_*\\s])+?(?:\\s|[*]))([a-zA-Z$_][a-zA-Z0-9$_]*)(\\s*\\([^;]*?\\))((?:\\s|//.*?\\n|/[*].*?[*]/)+)(;)" (ByGroups [(Using lexer), (Arbitrary "Name" :. Arbitrary "Function"), (Using lexer), (Arbitrary "Text"), (Arbitrary "Punctuation")])
    , tokNext "(@interface|@implementation)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tokNext "(@class|@protocol)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo forward_classname')
    , tok "(\\s*)(@end)(\\s*)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text")])
    , tokNext "" (Arbitrary "Text") (GoTo statement')
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

