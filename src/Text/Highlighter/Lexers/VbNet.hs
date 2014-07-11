module Text.Highlighter.Lexers.VbNet (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "VB.net"
    , lAliases = ["vb.net", "vbnet"]
    , lExtensions = [".vb", ".bas"]
    , lMimetypes = ["text/x-vbnet", "text/x-vba"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

dim' :: TokenMatcher
dim' =
    [ tokNext "[a-z_][a-z0-9_]*" (Arbitrary "Name" :. Arbitrary "Variable") Pop
    , tokNext "" (Arbitrary "Text") Pop
    ]

end' :: TokenMatcher
end' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "(Function|Sub|Property|Class|Structure|Enum|Module|Namespace)\\b" (Arbitrary "Keyword") Pop
    , tokNext "" (Arbitrary "Text") Pop
    ]

string' :: TokenMatcher
string' =
    [ tok "\"\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\"C?" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "[^\"]+" (Arbitrary "Literal" :. Arbitrary "String")
    ]

namespace' :: TokenMatcher
namespace' =
    [ tokNext "[a-z_][a-z0-9_.]*" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

funcname' :: TokenMatcher
funcname' =
    [ tokNext "[a-z_][a-z0-9_]*" (Arbitrary "Name" :. Arbitrary "Function") Pop
    ]

classname' :: TokenMatcher
classname' =
    [ tokNext "[a-z_][a-z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "^\\s*<.*?>" (Arbitrary "Name" :. Arbitrary "Attribute")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\n" (Arbitrary "Text")
    , tok "rem\\b.*?\\n" (Arbitrary "Comment")
    , tok "'.*?\\n" (Arbitrary "Comment")
    , tok "#If\\s.*?\\sThen|#ElseIf\\s.*?\\sThen|#End\\s+If|#Const|#ExternalSource.*?\\n|#End\\s+ExternalSource|#Region.*?\\n|#End\\s+Region|#ExternalChecksum" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "[\\(\\){}!#,.:]" (Arbitrary "Punctuation")
    , tok "Option\\s+(Strict|Explicit|Compare)\\s+(On|Off|Binary|Text)" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "(?<!\\.)(AddHandler|Alias|ByRef|ByVal|Call|Case|Catch|CBool|CByte|CChar|CDate|CDec|CDbl|CInt|CLng|CObj|Continue|CSByte|CShort|CSng|CStr|CType|CUInt|CULng|CUShort|Declare|Default|Delegate|DirectCast|Do|Each|Else|ElseIf|EndIf|Erase|Error|Event|Exit|False|Finally|For|Friend|Get|Global|GoSub|GoTo|Handles|If|Implements|Inherits|Interface|Let|Lib|Loop|Me|MustInherit|MustOverride|MyBase|MyClass|Narrowing|New|Next|Not|Nothing|NotInheritable|NotOverridable|Of|On|Operator|Option|Optional|Overloads|Overridable|Overrides|ParamArray|Partial|Private|Protected|Public|RaiseEvent|ReadOnly|ReDim|RemoveHandler|Resume|Return|Select|Set|Shadows|Shared|Single|Static|Step|Stop|SyncLock|Then|Throw|To|True|Try|TryCast|Wend|Using|When|While|Widening|With|WithEvents|WriteOnly)\\b" (Arbitrary "Keyword")
    , tokNext "(?<!\\.)End\\b" (Arbitrary "Keyword") (GoTo end')
    , tokNext "(?<!\\.)(Dim|Const)\\b" (Arbitrary "Keyword") (GoTo dim')
    , tokNext "(?<!\\.)(Function|Sub|Property)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo funcname')
    , tokNext "(?<!\\.)(Class|Structure|Enum)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tokNext "(?<!\\.)(Module|Namespace|Imports)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo namespace')
    , tok "(?<!\\.)(Boolean|Byte|Char|Date|Decimal|Double|Integer|Long|Object|SByte|Short|Single|String|Variant|UInteger|ULong|UShort)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(?<!\\.)(AddressOf|And|AndAlso|As|GetType|In|Is|IsNot|Like|Mod|Or|OrElse|TypeOf|Xor)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "&=|[*]=|/=|\\\\=|\\^=|\\+=|-=|<<=|>>=|<<|>>|:=|<=|>=|<>|[-&*/\\\\^+=<>]" (Arbitrary "Operator")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "[a-zA-Z_][a-zA-Z0-9_]*[%&@!#$]?" (Arbitrary "Name")
    , tok "#.*?#" (Arbitrary "Literal" :. Arbitrary "Date")
    , tok "(\\d+\\.\\d*|\\d*\\.\\d+)([fF][+-]?[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+([SILDFR]|US|UI|UL)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "&H[0-9a-f]+([SILDFR]|US|UI|UL)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "&O[0-7]+([SILDFR]|US|UI|UL)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "_\\n" (Arbitrary "Text")
    ]

