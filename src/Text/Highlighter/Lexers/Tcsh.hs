module Text.Highlighter.Lexers.Tcsh (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Tcsh"
    , lAliases = ["tcsh", "csh"]
    , lExtensions = [".tcsh", ".csh"]
    , lMimetypes = ["application/x-csh"]
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
    , tokNext "\\$\\(" (Arbitrary "Keyword") (GoTo paren')
    , tokNext "\\${#?" (Arbitrary "Keyword") (GoTo curly')
    , tokNext "`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick") (GoTo backticks')
    , anyOf data'
    ]

basic' :: TokenMatcher
basic' =
    [ tok "\\b(if|endif|else|while|then|foreach|case|default|continue|goto|breaksw|end|switch|endsw)\\s*\\b" (Arbitrary "Keyword")
    , tok "\\b(alias|alloc|bg|bindkey|break|builtins|bye|caller|cd|chdir|complete|dirs|echo|echotc|eval|exec|exit|fg|filetest|getxvers|glob|getspath|hashstat|history|hup|inlib|jobs|kill|limit|log|login|logout|ls-F|migrate|newgrp|nice|nohup|notify|onintr|popd|printenv|pushd|rehash|repeat|rootnode|popd|pushd|set|shift|sched|setenv|setpath|settc|setty|setxvers|shift|source|stop|suspend|source|suspend|telltc|time|umask|unalias|uncomplete|unhash|universe|unlimit|unset|unsetenv|ver|wait|warp|watchlog|where|which)\\s*\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "#.*\\n" (Arbitrary "Comment")
    , tok "\\\\[\\w\\W]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "(\\b\\w+)(\\s*)(=)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tok "[\\[\\]{}()=]+" (Arbitrary "Operator")
    , tok "<<\\s*(\\'?)\\\\?(\\w+)[\\w\\W]+?\\2" (Arbitrary "Literal" :. Arbitrary "String")
    ]

paren' :: TokenMatcher
paren' =
    [ tokNext "\\)" (Arbitrary "Keyword") Pop
    , anyOf root'
    ]

data' :: TokenMatcher
data' =
    [ tok "(?s)\"(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "(?s)'(\\\\\\\\|\\\\[0-7]+|\\\\.|[^'\\\\])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "\\s+" (Arbitrary "Text")
    , tok "[^=\\s\\n\\[\\]{}()$\"\\'`\\\\]+" (Arbitrary "Text")
    , tok "\\d+(?= |\\Z)" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\$#?(\\w+|.)" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

