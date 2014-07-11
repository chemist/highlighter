module Text.Highlighter.Lexers.ProtoBuf (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Protocol Buffer"
    , lAliases = ["protobuf"]
    , lExtensions = [".proto"]
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

type' :: TokenMatcher
type' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name") Pop
    ]

message' :: TokenMatcher
message' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "[ \\t]+" (Arbitrary "Text")
    , tok "[,;{}\\[\\]\\(\\)]" (Arbitrary "Punctuation")
    , tok "/(\\\\\\n)?/(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "\\b(import|option|optional|required|repeated|default|packed|ctype|extensions|to|max|rpc|returns)\\b" (Arbitrary "Keyword")
    , tok "(int32|int64|uint32|uint64|sint32|sint64|fixed32|fixed64|sfixed32|sfixed64|float|double|bool|string|bytes)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(true|false)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tokNext "(package)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")]) (GoTo package')
    , tokNext "(message|extend)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text")]) (GoTo message')
    , tokNext "(enum|group|service)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text")]) (GoTo type')
    , tok "\\\".*\\\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+)[eE][+-]?\\d+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\-?(inf|nan))" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-fA-F]+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\d+[LlUu]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[+-=]" (Arbitrary "Operator")
    , tok "([a-zA-Z_][a-zA-Z0-9_\\.]*)([ \\t]*)(=)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Attribute"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tok "[a-zA-Z_][a-zA-Z0-9_\\.]*" (Arbitrary "Name")
    ]

package' :: TokenMatcher
package' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

