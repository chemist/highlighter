module Text.Highlighter.Lexers.Ada (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Ada"
    , lAliases = ["ada", "ada95ada2005"]
    , lExtensions = [".adb", ".ads", ".ada"]
    , lMimetypes = ["text/x-ada"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

package_instantiation' :: TokenMatcher
package_instantiation' =
    [ tok "(\"[^\"]+\"|[a-z0-9_]+)(\\s+)(=>)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text"), (Arbitrary "Punctuation")])
    , tok "[a-z0-9._\\'\"]" (Arbitrary "Text")
    , tokNext "\\)" (Arbitrary "Punctuation") Pop
    , anyOf root'
    ]

formal_part' :: TokenMatcher
formal_part' =
    [ tokNext "\\)" (Arbitrary "Punctuation") Pop
    , tok "([a-z0-9_]+)(\\s*)(,|:[^=])" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text"), (Arbitrary "Punctuation")])
    , tok "(in|not|null|out|access)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , anyOf root'
    ]

attribute' :: TokenMatcher
attribute' =
    [ tok "(')([a-zA-Z0-9_]+)" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Attribute")])
    ]

subprogram' :: TokenMatcher
subprogram' =
    [ tokNext "\\(" (Arbitrary "Punctuation") (DoAll [Pop, (GoTo formal_part')])
    , tokNext ";" (Arbitrary "Punctuation") Pop
    , tokNext "is\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved") Pop
    , tok "\"[^\"]+\"|[a-z0-9_]+" (Arbitrary "Name" :. Arbitrary "Function")
    , anyOf root'
    ]

numbers' :: TokenMatcher
numbers' =
    [ tok "[0-9_]+#[0-9a-f]+#" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9_]+\\.[0-9_]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9_]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    ]

type_def' :: TokenMatcher
type_def' =
    [ tokNext ";" (Arbitrary "Punctuation") Pop
    , tokNext "\\(" (Arbitrary "Punctuation") (GoTo formal_part')
    , tok "with|and|use" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tokNext "array\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved") (DoAll [Pop, (GoTo array_def')])
    , tokNext "record\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved") (GoTo formal_part')
    , anyOf root'
    ]

end' :: TokenMatcher
end' =
    [ tok "(if|case|record|loop|select)" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "\"[^\"]+\"|[a-zA-Z0-9_]+" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "[\10\\s]+" (Arbitrary "Text")
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

array_def' :: TokenMatcher
array_def' =
    [ tokNext ";" (Arbitrary "Punctuation") Pop
    , tok "([a-z0-9_]+)(\\s+)(range)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Reserved")])
    , anyOf root'
    ]

package' :: TokenMatcher
package' =
    [ tok "body" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "is\\s+new|renames" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tokNext "is" (Arbitrary "Keyword" :. Arbitrary "Reserved") Pop
    , tokNext ";" (Arbitrary "Punctuation") Pop
    , tokNext "\\(" (Arbitrary "Punctuation") (GoTo package_instantiation')
    , tok "([a-zA-Z0-9_.]+)" (Arbitrary "Name" :. Arbitrary "Class")
    , anyOf root'
    ]

import' :: TokenMatcher
import' =
    [ tokNext "[a-z0-9_.]+" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "[^\\S\\n]+" (Arbitrary "Text")
    , tok "--.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "[^\\S\\n]+" (Arbitrary "Text")
    , tokNext "function|procedure|entry" (Arbitrary "Keyword" :. Arbitrary "Declaration") (GoTo subprogram')
    , tokNext "(subtype|type)(\\s+)([a-z0-9_]+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type")]) (GoTo type_def')
    , tok "task|protected" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "(subtype)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text")])
    , tokNext "(end)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Reserved"), (Arbitrary "Text")]) (GoTo end')
    , tok "(pragma)(\\s+)([a-zA-Z0-9_]+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Reserved"), (Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tok "(true|false|null)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(Byte|Character|Float|Integer|Long_Float|Long_Integer|Long_Long_Float|Long_Long_Integer|Natural|Positive|Short_Float|Short_Integer|Short_Short_Float|Short_Short_Integer|String|Wide_String|Duration)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(and(\\s+then)?|in|mod|not|or(\\s+else)|rem)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "generic|private" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tokNext "package" (Arbitrary "Keyword" :. Arbitrary "Declaration") (GoTo package')
    , tokNext "array\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved") (GoTo array_def')
    , tokNext "(with|use)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")]) (GoTo import')
    , tok "([a-z0-9_]+)(\\s*)(:)(\\s*)(constant)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Constant"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Reserved")])
    , tok "<<[a-z0-9_]+>>" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "([a-z0-9_]+)(\\s*)(:)(\\s*)(declare|begin|loop|for|while)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Reserved")])
    , tok "\\b(abort|abs|abstract|accept|access|aliased|all|array|at|begin|body|case|constant|declare|delay|delta|digits|do|else|elsif|end|entry|exception|exit|interface|for|goto|if|is|limited|loop|new|null|of|or|others|out|overriding|pragma|protected|raise|range|record|renames|requeue|return|reverse|select|separate|subtype|synchronized|task|tagged|terminate|then|type|until|when|while|xor)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "\"[^\"]*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , anyOf attribute'
    , anyOf numbers'
    , tok "'[^']'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Character")
    , tok "([a-z0-9_]+)(\\s*|[(,])" (ByGroups [(Arbitrary "Name"), (Using lexer)])
    , tok "(<>|=>|:=|[\\(\\)\\|:;,.'])" (Arbitrary "Punctuation")
    , tok "[*<>+=/&-]" (Arbitrary "Operator")
    , tok "\\n+" (Arbitrary "Text")
    ]

