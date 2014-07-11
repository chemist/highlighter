module Text.Highlighter.Lexers.Modelica (lexer) where
import qualified Text.Highlighter.Lexers.Html as Html
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Modelica"
    , lAliases = ["modelica"]
    , lExtensions = [".mo"]
    , lMimetypes = ["text/x-modelica"]
    , lStart = root'
    , lFlags = [caseless, dotall]
    }

functions' :: TokenMatcher
functions' =
    [ tok "(abs|acos|acosh|asin|asinh|atan|atan2|atan3|ceil|cos|cosh|cross|div|exp|floor|log|log10|mod|rem|sign|sin|sinh|size|sqrt|tan|tanh|zeros)\\b" (Arbitrary "Name" :. Arbitrary "Function")
    ]

classes' :: TokenMatcher
classes' =
    [ tok "(block|class|connector|function|model|package|record|type)\\b" (Arbitrary "Name" :. Arbitrary "Class")
    ]

statements' :: TokenMatcher
statements' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+|\\d.)[eE][+-]?\\d+[lL]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[\126!%^&*+=|?:<>/-]" (Arbitrary "Operator")
    , tok "[()\\[\\]{},.;]" (Arbitrary "Punctuation")
    , tok "(true|false|NULL|Real|Integer|Boolean)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "([a-zA-Z_][\\w]*|'[a-zA-Z_\\+\\-\\*\\/\\^][\\w]*')(\\.([a-zA-Z_][\\w]*|'[a-zA-Z_\\+\\-\\*\\/\\^][\\w]*'))+" (Arbitrary "Name" :. Arbitrary "Class")
    , tok "('[\\w\\+\\-\\*\\/\\^]+'|\\w+)" (Arbitrary "Name")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "//(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment")
    ]

htmlContent' :: TokenMatcher
htmlContent' =
    [ tokNext "<\\s*/\\s*html\\s*>" (Arbitrary "Name" :. Arbitrary "Tag") Pop
    , tok ".+?(?=<\\s*/\\s*html\\s*>)" (Using Html.lexer)
    ]

keywords' :: TokenMatcher
keywords' =
    [ tok "(algorithm|annotation|break|connect|constant|constrainedby|discrete|each|else|elseif|elsewhen|encapsulated|enumeration|end|equation|exit|expandable|extends|external|false|final|flow|for|if|import|in|inner|input|loop|nondiscrete|outer|output|parameter|partial|protected|public|redeclare|replaceable|stream|time|then|true|when|while|within)\\b" (Arbitrary "Keyword")
    ]

operators' :: TokenMatcher
operators' =
    [ tok "(and|assert|cardinality|change|delay|der|edge|initial|noEvent|not|or|pre|reinit|return|sample|smooth|terminal|terminate)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , anyOf keywords'
    , anyOf functions'
    , anyOf operators'
    , anyOf classes'
    , tokNext "(\"<html>|<html>)" (Arbitrary "Name" :. Arbitrary "Tag") (GoTo htmlContent')
    , anyOf statements'
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

