module Text.Highlighter.Lexers.Tcl (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Tcl"
    , lAliases = ["tcl"]
    , lExtensions = [".tcl"]
    , lMimetypes = ["text/x-tcl", "text/x-script.tcl", "application/x-tcl"]
    , lStart = root'
    , lFlags = [multiline]
    }

comment' :: TokenMatcher
comment' =
    [ tokNext ".*[^\\\\]\\n" (Arbitrary "Comment") Pop
    , tok ".*\\\\\\n" (Arbitrary "Comment")
    ]

stringSquare' :: TokenMatcher
stringSquare' =
    [ tokNext "\\[" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo stringSquare')
    , tok "(?s)(\\\\\\\\|\\\\[0-7]+|\\\\.|\\\\\\n|[^\\]\\\\])" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tokNext "\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") Pop
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\\[" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo stringSquare')
    , tok "(?s)(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\])" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") Pop
    ]

commandInParen' :: TokenMatcher
commandInParen' =
    [ tokNext "\\b(after|apply|array|break|catch|continue|elseif|else|error|eval|expr|for|foreach|global|if|namespace|proc|rename|return|set|switch|then|trace|unset|update|uplevel|upvar|variable|vwait|while)\\b" (Arbitrary "Keyword") (GoTo paramsInParen')
    , tokNext "\\b(append|bgerror|binary|cd|chan|clock|close|concat|dde|dict|encoding|eof|exec|exit|fblocked|fconfigure|fcopy|file|fileevent|flush|format|gets|glob|history|http|incr|info|interp|join|lappend|lassign|lindex|linsert|list|llength|load|loadTk|lrange|lrepeat|lreplace|lreverse|lsearch|lset|lsort|mathfunc|mathop|memory|msgcat|open|package|pid|pkg::create|pkg_mkIndex|platform|platform::shell|puts|pwd|re_syntax|read|refchan|regexp|registry|regsub|scan|seek|socket|source|split|string|subst|tell|time|tm|unknown|unload)\\b" (Arbitrary "Name" :. Arbitrary "Builtin") (GoTo paramsInParen')
    , tokNext "([\\w\\.\\-]+)" (Arbitrary "Name" :. Arbitrary "Variable") (GoTo paramsInParen')
    , tokNext "#" (Arbitrary "Comment") (GoTo comment')
    ]

data' :: TokenMatcher
data' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "0x[a-fA-F0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\d+\\.\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\$([\\w\\.\\-\\:]+)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "([\\w\\.\\-\\:]+)" (Arbitrary "Text")
    ]

brace' :: TokenMatcher
brace' =
    [ tokNext "}" (Arbitrary "Keyword") Pop
    , anyOf commandInBrace'
    , anyOf basic'
    , anyOf data'
    ]

paren' :: TokenMatcher
paren' =
    [ tokNext "\\)" (Arbitrary "Keyword") Pop
    , anyOf commandInParen'
    , anyOf basic'
    , anyOf data'
    ]

paramsInParen' :: TokenMatcher
paramsInParen' =
    [ tokNext "\\)" (Arbitrary "Keyword") (DoAll [Pop, Pop])
    , anyOf params'
    ]

bracket' :: TokenMatcher
bracket' =
    [ tokNext "\\]" (Arbitrary "Keyword") Pop
    , anyOf commandInBracket'
    , anyOf basic'
    , anyOf data'
    ]

command' :: TokenMatcher
command' =
    [ tokNext "\\b(after|apply|array|break|catch|continue|elseif|else|error|eval|expr|for|foreach|global|if|namespace|proc|rename|return|set|switch|then|trace|unset|update|uplevel|upvar|variable|vwait|while)\\b" (Arbitrary "Keyword") (GoTo params')
    , tokNext "\\b(append|bgerror|binary|cd|chan|clock|close|concat|dde|dict|encoding|eof|exec|exit|fblocked|fconfigure|fcopy|file|fileevent|flush|format|gets|glob|history|http|incr|info|interp|join|lappend|lassign|lindex|linsert|list|llength|load|loadTk|lrange|lrepeat|lreplace|lreverse|lsearch|lset|lsort|mathfunc|mathop|memory|msgcat|open|package|pid|pkg::create|pkg_mkIndex|platform|platform::shell|puts|pwd|re_syntax|read|refchan|regexp|registry|regsub|scan|seek|socket|source|split|string|subst|tell|time|tm|unknown|unload)\\b" (Arbitrary "Name" :. Arbitrary "Builtin") (GoTo params')
    , tokNext "([\\w\\.\\-]+)" (Arbitrary "Name" :. Arbitrary "Variable") (GoTo params')
    , tokNext "#" (Arbitrary "Comment") (GoTo comment')
    ]

params' :: TokenMatcher
params' =
    [ tokNext ";" (Arbitrary "Keyword") Pop
    , tokNext "\\n" (Arbitrary "Text") Pop
    , tok "(else|elseif|then)" (Arbitrary "Keyword")
    , anyOf basic'
    , anyOf data'
    ]

basic' :: TokenMatcher
basic' =
    [ tokNext "\\(" (Arbitrary "Keyword") (GoTo paren')
    , tokNext "\\[" (Arbitrary "Keyword") (GoTo bracket')
    , tokNext "\\{" (Arbitrary "Keyword") (GoTo brace')
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo string')
    , tok "(eq|ne|in|ni)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "!=|==|<<|>>|<=|>=|&&|\\|\\||\\*\\*|[-+\126!*/%<>&^|?:]" (Arbitrary "Operator")
    ]

paramsInBracket' :: TokenMatcher
paramsInBracket' =
    [ tokNext "\\]" (Arbitrary "Keyword") (DoAll [Pop, Pop])
    , anyOf params'
    ]

commandInBracket' :: TokenMatcher
commandInBracket' =
    [ tokNext "\\b(after|apply|array|break|catch|continue|elseif|else|error|eval|expr|for|foreach|global|if|namespace|proc|rename|return|set|switch|then|trace|unset|update|uplevel|upvar|variable|vwait|while)\\b" (Arbitrary "Keyword") (GoTo paramsInBracket')
    , tokNext "\\b(append|bgerror|binary|cd|chan|clock|close|concat|dde|dict|encoding|eof|exec|exit|fblocked|fconfigure|fcopy|file|fileevent|flush|format|gets|glob|history|http|incr|info|interp|join|lappend|lassign|lindex|linsert|list|llength|load|loadTk|lrange|lrepeat|lreplace|lreverse|lsearch|lset|lsort|mathfunc|mathop|memory|msgcat|open|package|pid|pkg::create|pkg_mkIndex|platform|platform::shell|puts|pwd|re_syntax|read|refchan|regexp|registry|regsub|scan|seek|socket|source|split|string|subst|tell|time|tm|unknown|unload)\\b" (Arbitrary "Name" :. Arbitrary "Builtin") (GoTo paramsInBracket')
    , tokNext "([\\w\\.\\-]+)" (Arbitrary "Name" :. Arbitrary "Variable") (GoTo paramsInBracket')
    , tokNext "#" (Arbitrary "Comment") (GoTo comment')
    ]

paramsInBrace' :: TokenMatcher
paramsInBrace' =
    [ tokNext "}" (Arbitrary "Keyword") (DoAll [Pop, Pop])
    , anyOf params'
    ]

root' :: TokenMatcher
root' =
    [ anyOf command'
    , anyOf basic'
    , anyOf data'
    , tok "}" (Arbitrary "Keyword")
    ]

commandInBrace' :: TokenMatcher
commandInBrace' =
    [ tokNext "\\b(after|apply|array|break|catch|continue|elseif|else|error|eval|expr|for|foreach|global|if|namespace|proc|rename|return|set|switch|then|trace|unset|update|uplevel|upvar|variable|vwait|while)\\b" (Arbitrary "Keyword") (GoTo paramsInBrace')
    , tokNext "\\b(append|bgerror|binary|cd|chan|clock|close|concat|dde|dict|encoding|eof|exec|exit|fblocked|fconfigure|fcopy|file|fileevent|flush|format|gets|glob|history|http|incr|info|interp|join|lappend|lassign|lindex|linsert|list|llength|load|loadTk|lrange|lrepeat|lreplace|lreverse|lsearch|lset|lsort|mathfunc|mathop|memory|msgcat|open|package|pid|pkg::create|pkg_mkIndex|platform|platform::shell|puts|pwd|re_syntax|read|refchan|regexp|registry|regsub|scan|seek|socket|source|split|string|subst|tell|time|tm|unknown|unload)\\b" (Arbitrary "Name" :. Arbitrary "Builtin") (GoTo paramsInBrace')
    , tokNext "([\\w\\.\\-]+)" (Arbitrary "Name" :. Arbitrary "Variable") (GoTo paramsInBrace')
    , tokNext "#" (Arbitrary "Comment") (GoTo comment')
    ]

