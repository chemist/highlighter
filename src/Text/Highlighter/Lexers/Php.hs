module Text.Highlighter.Lexers.Php (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "PHP"
    , lAliases = ["php", "php3", "php4", "php5"]
    , lExtensions = [".php", ".php[345]"]
    , lMimetypes = ["text/x-php"]
    , lStart = root'
    , lFlags = [caseless, multiline, dotall]
    }

classname' :: TokenMatcher
classname' =
    [ tokNext "[a-zA-Z_][\\\\a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

php' :: TokenMatcher
php' =
    [ tokNext "\\?>" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok "<<<(\\'?)([a-zA-Z_][a-zA-Z0-9_]*)\\1\\n.*?\\n\\2\\;?\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\s+" (Arbitrary "Text")
    , tok "#.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "/\\*\\*.*?\\*/" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Doc")
    , tok "/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "(->|::)(\\s*)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Attribute")])
    , tok "[\126!%^&*+=|:.<>/?@-]+" (Arbitrary "Operator")
    , tok "[\\[\\]{}();,]+" (Arbitrary "Punctuation")
    , tokNext "(class)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tok "(function)(\\s*)(?=\\()" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")])
    , tokNext "(function)(\\s+)(&?)(\\s*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Operator"), (Arbitrary "Text")]) (GoTo functionname')
    , tok "(const)(\\s+)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Constant")])
    , tok "(and|E_PARSE|old_function|E_ERROR|or|as|E_WARNING|parent|eval|PHP_OS|break|exit|case|extends|PHP_VERSION|cfunction|FALSE|print|for|require|continue|foreach|require_once|declare|return|default|static|do|switch|die|stdClass|echo|else|TRUE|elseif|var|empty|if|xor|enddeclare|include|virtual|endfor|include_once|while|endforeach|global|__FILE__|endif|list|__LINE__|endswitch|new|__sleep|endwhile|not|array|__wakeup|E_ALL|NULL|final|php_user_filter|interface|implements|public|private|protected|abstract|clone|try|catch|throw|this|use|namespace)\\b" (Arbitrary "Keyword")
    , tok "(true|false|null)\8" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "\\$\\{\\$+[a-zA-Z_][a-zA-Z0-9_]*\\}" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "\\$+[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[\\\\a-zA-Z_][\\\\a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Other")
    , tok "(\\d+\\.\\d*|\\d*\\.\\d+)([eE][+-]?[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+[eE][+-]?[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[0-7]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[xX][a-fA-F0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "'([^'\\\\]*(?:\\\\.[^'\\\\]*)*)'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "`([^`\\\\]*(?:\\\\.[^`\\\\]*)*)`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo string')
    ]

root' :: TokenMatcher
root' =
    [ tokNext "<\\?(php)?" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo php')
    , tok "[^<]+" (Arbitrary "Other")
    , tok "<" (Arbitrary "Other")
    ]

functionname' :: TokenMatcher
functionname' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Function") Pop
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") Pop
    , tok "[^{$\"\\\\]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "\\\\([nrt\\\"$\\\\]|[0-7]{1,3}|x[0-9A-Fa-f]{1,2})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "\\$[a-zA-Z_][a-zA-Z0-9_]*(\\[\\S+\\]|->[a-zA-Z_][a-zA-Z0-9_]*)?" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol")
    , tok "(\\{\\$\\{)(.*?)(\\}\\})" (ByGroups [(Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol"), (Using lexer), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol")])
    , tok "(\\{)(\\$.*?)(\\})" (ByGroups [(Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol"), (Using lexer), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol")])
    , tok "(\\$\\{)(\\S+)(\\})" (ByGroups [(Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol"), (Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol")])
    , tok "[${\\\\]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    ]

