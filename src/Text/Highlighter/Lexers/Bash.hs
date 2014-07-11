module Text.Highlighter.Lexers.Bash (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Bash"
    , lAliases = ["bash", "sh", "ksh"]
    , lExtensions = [".sh", ".ksh", ".bash", ".ebuild", ".eclass"]
    , lMimetypes = ["application/x-sh", "application/x-shellscript"]
    , lStart = root'
    , lFlags = [multiline]
    }

curly' :: TokenMatcher
curly' =
    [ tokNext "}" (Arbitrary "Keyword") Pop
    , tok ":-" (Arbitrary "Keyword")
    , tok "[a-zA-Z0-9_]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[^}:\"\\'`$]+" (Arbitrary "Punctuation")
    , tok ":" (Arbitrary "Punctuation")
    , anyOf root'
    ]

backticks' :: TokenMatcher
backticks' =
    [ tokNext "`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick") Pop
    , anyOf root'
    ]

root' :: TokenMatcher
root' =
    [ anyOf basic'
    , tokNext "\\$\\(\\(" (Arbitrary "Keyword") (GoTo math')
    , tokNext "\\$\\(" (Arbitrary "Keyword") (GoTo paren')
    , tokNext "\\${#?" (Arbitrary "Keyword") (GoTo curly')
    , tokNext "`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick") (GoTo backticks')
    , anyOf data'
    ]

basic' :: TokenMatcher
basic' =
    [ tok "\\b(if|fi|else|while|do|done|for|then|return|function|case|select|continue|until|esac|elif)\\s*\\b" (Arbitrary "Keyword")
    , tok "\\b(alias|bg|bind|break|builtin|caller|cd|command|compgen|complete|declare|dirs|disown|echo|enable|eval|exec|exit|export|false|fc|fg|getopts|hash|help|history|jobs|kill|let|local|logout|popd|printf|pushd|pwd|read|readonly|set|shift|shopt|source|suspend|test|time|times|trap|true|type|typeset|ulimit|umask|unalias|unset|wait)\\s*\\b(?!\\.)" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "#.*\\n" (Arbitrary "Comment")
    , tok "\\\\[\\w\\W]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "(\\b\\w+)(\\s*)(=)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tok "[\\[\\]{}()=]" (Arbitrary "Operator")
    , tok "<<-?\\s*(\\'?)\\\\?(\\w+)[\\w\\W]+?\\2" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "&&|\\|\\|" (Arbitrary "Operator")
    ]

paren' :: TokenMatcher
paren' =
    [ tokNext "\\)" (Arbitrary "Keyword") Pop
    , anyOf root'
    ]

data' :: TokenMatcher
data' =
    [ tok "(?s)\\$?\"(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "(?s)\\$?'(\\\\\\\\|\\\\[0-7]+|\\\\.|[^'\\\\])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok ";" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "[^=\\s\\n\\[\\]{}()$\"\\'`\\\\<]+" (Arbitrary "Text")
    , tok "\\d+(?= |\\Z)" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\$#?(\\w+|.)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "<" (Arbitrary "Text")
    ]

math' :: TokenMatcher
math' =
    [ tokNext "\\)\\)" (Arbitrary "Keyword") Pop
    , tok "[-+*/%^|&]|\\*\\*|\\|\\|" (Arbitrary "Operator")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number")
    , anyOf root'
    ]

