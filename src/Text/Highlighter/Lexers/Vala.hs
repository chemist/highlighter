module Text.Highlighter.Lexers.Vala (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Vala"
    , lAliases = ["vala", "vapi"]
    , lExtensions = [".vala", ".vapi"]
    , lMimetypes = ["text/x-vala"]
    , lStart = root'
    , lFlags = [multiline]
    }

statements' :: TokenMatcher
statements' =
    [ tokNext "L?\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "L?'(\\\\.|\\\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\'\\n])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+)[eE][+-]?\\d+[lL]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-fA-F]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\d+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[\126!%^&*+=|?:<>/-]" (Arbitrary "Operator")
    , tok "(\\[)(Compact|Immutable|(?:Boolean|Simple)Type)(\\])" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Decorator"), (Arbitrary "Punctuation")])
    , tok "(\\[)(CCode|(?:Integer|Floating)Type)" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Decorator")])
    , tok "[()\\[\\],.]" (Arbitrary "Punctuation")
    , tok "(as|base|break|case|catch|construct|continue|default|delete|do|else|enum|finally|for|foreach|get|if|in|is|lock|new|out|params|return|set|sizeof|switch|this|throw|try|typeof|while|yield)\\b" (Arbitrary "Keyword")
    , tok "(abstract|const|delegate|dynamic|ensures|extern|inline|internal|override|owned|private|protected|public|ref|requires|signal|static|throws|unowned|var|virtual|volatile|weak|yields)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tokNext "(namespace|using)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")]) (GoTo namespace')
    , tokNext "(class|errordomain|interface|struct)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text")]) (GoTo class')
    , tok "(\\.)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Name" :. Arbitrary "Attribute")])
    , tok "(void|bool|char|double|float|int|int8|int16|int32|int64|long|short|size_t|ssize_t|string|time_t|uchar|uint|uint8|uint16|uint32|uint64|ulong|unichar|ushort)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(true|false|null)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tokNext "^\\s*#if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "//(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

statement' :: TokenMatcher
statement' =
    [ anyOf whitespace'
    , anyOf statements'
    , tok "[{}]" (Arbitrary "Punctuation")
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

if0' :: TokenMatcher
if0' =
    [ tokNext "^\\s*#if.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Push
    , tokNext "^\\s*#el(?:se|if).*\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tokNext "^\\s*#endif.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok ".*?\\n" (Arbitrary "Comment")
    ]

namespace' :: TokenMatcher
namespace' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_.]*" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tokNext "" (Arbitrary "Text") (GoTo statement')
    ]

class' :: TokenMatcher
class' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

