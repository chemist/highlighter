module Text.Highlighter.Lexers.BlitzMax (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "BlitzMax"
    , lAliases = ["blitzmax", "bmax"]
    , lExtensions = [".bmx"]
    , lMimetypes = ["text/x-bmx"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "[ \\t]+" (Arbitrary "Text")
    , tok "\\.\\.\\n" (Arbitrary "Text")
    , tok "'.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "([ \\t]*)\\bRem\\n(\\n|.)*?\\s*\\bEnd([ \\t]*)Rem" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo string')
    , tok "[0-9]+\\.[0-9]*(?!\\.)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\.[0-9]*(?!\\.)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\$[0-9a-f]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "\\%[10]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "(?:(?:(:)?([ \\t]*)(:?\\b(Shl|Shr|Sar|Mod)\\b|([+\\-*/&|\126]))|Or|And|Not|[=<>^]))" (Arbitrary "Operator")
    , tok "[(),.:\\[\\]]" (Arbitrary "Punctuation")
    , tok "(?:#[\\w \\t]*)" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "(?:\\?[\\w \\t]*)" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "\\b(New)\\b([ \\t]?)([(]?)([a-z_][a-z0-9_]*)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Reserved"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Class")])
    , tok "\\b(Import|Framework|Module)([ \\t]+)([a-z_][a-z0-9_]*\\.[a-z_][a-z0-9_]*)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Reserved"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Namespace")])
    , tok "([a-z_][a-z0-9_]*)(?:(?:([ \\t]*)(@{1,2}|[!#$%])|([ \\t]*:[ \\t]*\\b(?:Shl|Shr|Sar|Mod)\\b)|([ \\t]*)([:])([ \\t]*)(?:\\b(Int|Byte|Short|Float|Double|Long)\\b|([a-z_][a-z0-9_]*)))(?:([ \\t]*)(Ptr))?)?((?:[ \\t]|\\.\\.\\n)*)([(])" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Text"), (Arbitrary "Punctuation")])
    , tok "([a-z_][a-z0-9_]*)(?:(?:([ \\t]*)(@{1,2}|[!#$%])|([ \\t]*:[ \\t]*\\b(?:Shl|Shr|Sar|Mod)\\b)|([ \\t]*)([:])([ \\t]*)(?:\\b(Int|Byte|Short|Float|Double|Long)\\b|([a-z_][a-z0-9_]*)))(?:([ \\t]*)(Ptr))?)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Type")])
    , tok "\\b(Type|Extends)([ \\t]+)([a-z_][a-z0-9_]*)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Reserved"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Class")])
    , tok "\\b(Ptr)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "\\b(Pi|True|False|Null|Self|Super)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "\\b(Local|Global|Const|Field)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "\\b(TNullMethodException|TNullFunctionException|TNullObjectException|TArrayBoundsException|TRuntimeException)\\b" (Arbitrary "Name" :. Arbitrary "Exception")
    , tok "\\b(Strict|SuperStrict|Module|ModuleInfo|End|Return|Continue|Exit|Public|Private|Var|VarPtr|Chr|Len|Asc|SizeOf|Sgn|Abs|Min|Max|New|Release|Delete|Incbin|IncbinPtr|IncbinLen|Framework|Include|Import|Extern|EndExtern|Function|EndFunction|Type|EndType|Extends|Method|EndMethod|Abstract|Final|If|Then|Else|ElseIf|EndIf|For|To|Next|Step|EachIn|While|Wend|EndWhile|Repeat|Until|Forever|Select|Case|Default|EndSelect|Try|Catch|EndTry|Throw|Assert|Goto|DefData|ReadData|RestoreData)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "([a-z_][a-z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

string' :: TokenMatcher
string' =
    [ tok "\"\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tokNext "\"C?" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") Pop
    , tok "[^\"]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    ]

