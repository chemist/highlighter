module Text.Highlighter.Lexers.Asymptote (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Asymptote"
    , lAliases = ["asy", "asymptote"]
    , lExtensions = [".asy"]
    , lMimetypes = ["text/x-asymptote"]
    , lStart = root'
    , lFlags = [multiline]
    }

function' :: TokenMatcher
function' =
    [ anyOf whitespace'
    , anyOf statements'
    , tok ";" (Arbitrary "Punctuation")
    , tokNext "{" (Arbitrary "Punctuation") Push
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

statements' :: TokenMatcher
statements' =
    [ tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "'" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+)[eE][+-]?\\d+[lL]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-fA-F]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\d+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[\126!%^&*+=|?:<>/-]" (Arbitrary "Operator")
    , tok "[()\\[\\],.]" (Arbitrary "Punctuation")
    , tok "\\b(case)(.+?)(:)" (ByGroups [(Arbitrary "Keyword"), (Using lexer), (Arbitrary "Text")])
    , tok "(and|controls|tension|atleast|curl|if|else|while|for|do|return|break|continue|struct|typedef|new|access|import|unravel|from|include|quote|static|public|private|restricted|this|explicit|true|false|null|cycle|newframe|operator)\\b" (Arbitrary "Keyword")
    , tok "(Braid|FitResult|Label|Legend|TreeNode|abscissa|arc|arrowhead|binarytree|binarytreeNode|block|bool|bool3|bounds|bqe|circle|conic|coord|coordsys|cputime|ellipse|file|filltype|frame|grid3|guide|horner|hsv|hyperbola|indexedTransform|int|inversion|key|light|line|linefit|marginT|marker|mass|object|pair|parabola|path|path3|pen|picture|point|position|projection|real|revolution|scaleT|scientific|segment|side|slice|splitface|string|surface|tensionSpecifier|ticklocate|ticksgridT|tickvalues|transform|transformation|tree|triangle|trilinear|triple|vector|vertex|void)(?=([ ]{1,}[a-zA-Z]))" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(Braid|FitResult|TreeNode|abscissa|arrowhead|block|bool|bool3|bounds|coord|frame|guide|horner|int|linefit|marginT|pair|pen|picture|position|real|revolution|slice|splitface|ticksgridT|tickvalues|tree|triple|vertex|void)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*:(?!:)" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "//(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment")
    ]

statement' :: TokenMatcher
statement' =
    [ anyOf whitespace'
    , anyOf statements'
    , tok "[{}]" (Arbitrary "Punctuation")
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tokNext "((?:[a-zA-Z0-9_*\\s])+?(?:\\s|[*]))([a-zA-Z_][a-zA-Z0-9_]*)(\\s*\\([^;]*?\\))((?:\\s|//.*?\\n|/[*].*?[*]/)+)({)" (ByGroups [(Using lexer), (Arbitrary "Name" :. Arbitrary "Function"), (Using lexer), (Using lexer), (Arbitrary "Punctuation")]) (GoTo function')
    , tok "((?:[a-zA-Z0-9_*\\s])+?(?:\\s|[*]))([a-zA-Z_][a-zA-Z0-9_]*)(\\s*\\([^;]*?\\))((?:\\s|//.*?\\n|/[*].*?[*]/)+)(;)" (ByGroups [(Using lexer), (Arbitrary "Name" :. Arbitrary "Function"), (Using lexer), (Using lexer), (Arbitrary "Punctuation")])
    , tokNext "" (Arbitrary "Text") (GoTo statement')
    ]

string' :: TokenMatcher
string' =
    [ tokNext "'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\'?]|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "[^\\\\'\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

