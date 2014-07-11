module Text.Highlighter.Lexers.Cpp (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "C++"
    , lAliases = ["cpp", "c++"]
    , lExtensions = [".cpp", ".hpp", ".c++", ".h++", ".cc", ".hh", ".cxx", ".hxx"]
    , lMimetypes = ["text/x-c++hdr", "text/x-c++src"]
    , lStart = root'
    , lFlags = [multiline]
    }

classname' :: TokenMatcher
classname' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    , tokNext "\\s*(?=>)" (Arbitrary "Text") Pop
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
    [ tokNext "^#if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tokNext "^#" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    , tokNext "^(?:\\s|//.*?\\n|/[*].*?[*]/)+#if\\s+0" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo if0')
    , tokNext "^(?:\\s|//.*?\\n|/[*].*?[*]/)+#" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    , tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "/(\\\\\\n)?/(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "[{}]" (Arbitrary "Punctuation")
    , tokNext "L?\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "L?'(\\\\.|\\\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\'\\n])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+)[eE][+-]?\\d+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-fA-F]+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\d+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\*/" (Arbitrary "Error")
    , tok "[\126!%^&*+=|?:<>/-]" (Arbitrary "Operator")
    , tok "[()\\[\\],.;]" (Arbitrary "Punctuation")
    , tok "(asm|auto|break|case|catch|const|const_cast|continue|default|delete|do|dynamic_cast|else|enum|explicit|export|extern|for|friend|goto|if|mutable|namespace|new|operator|private|protected|public|register|reinterpret_cast|return|restrict|sizeof|static|static_cast|struct|switch|template|this|throw|throws|try|typedef|typeid|typename|union|using|volatile|virtual|while)\\b" (Arbitrary "Keyword")
    , tokNext "(class)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tok "(bool|int|long|float|short|double|char|unsigned|signed|void|wchar_t)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(_{0,2}inline|naked|thread)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|declspec|finally|int64|try|leave|wchar_t|w64|virtual_inheritance|uuidof|unaligned|super|single_inheritance|raise|noop|multiple_inheritance|m128i|m128d|m128|m64|interface|identifier|forceinline|event|assume)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(__offload|__blockingoffload|__outer)\\b" (Arbitrary "Keyword" :. Arbitrary "Psuedo")
    , tok "(true|false)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "NULL\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*:(?!:)" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

if0' :: TokenMatcher
if0' =
    [ tokNext "^\\s*#if.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Push
    , tokNext "^\\s*#endif.*?(?<!\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok ".*?\\n" (Arbitrary "Comment")
    ]

