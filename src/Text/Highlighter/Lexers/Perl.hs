module Text.Highlighter.Lexers.Perl (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Perl"
    , lAliases = ["perl", "pl"]
    , lExtensions = [".pl", ".pm"]
    , lMimetypes = ["text/x-perl", "application/x-perl"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

modulename' :: TokenMatcher
modulename' =
    [ tokNext "[a-zA-Z_][\\w_]*" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

format' :: TokenMatcher
format' =
    [ tokNext "\\.\\n" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") Pop
    , tok "[^\\n]*\\n" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol")
    ]

rbString' :: TokenMatcher
rbString' =
    [ tok "\\\\[\\(\\)\\\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tokNext "\\(" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo rbString')
    , tokNext "\\)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") Pop
    , tok "[^\\(\\)]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    ]

balancedRegex' :: TokenMatcher
balancedRegex' =
    [ tokNext "/(\\\\\\\\|\\\\/|[^/])*/[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "!(\\\\\\\\|\\\\!|[^!])*![egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "\\\\(\\\\\\\\|[^\\\\])*\\\\[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "{(\\\\\\\\|\\\\}|[^}])*}[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "<(\\\\\\\\|\\\\>|[^>])*>[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "\\[(\\\\\\\\|\\\\\\]|[^\\]])*\\][egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "\\((\\\\\\\\|\\\\\\)|[^\\)])*\\)[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "@(\\\\\\\\|\\\\\\@|[^\\@])*@[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "%(\\\\\\\\|\\\\\\%|[^\\%])*%[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    , tokNext "\\$(\\\\\\\\|\\\\\\$|[^\\$])*\\$[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") Pop
    ]

name' :: TokenMatcher
name' =
    [ tok "[a-zA-Z0-9_]+::" (Arbitrary "Name" :. Arbitrary "Namespace")
    , tokNext "[a-zA-Z0-9_:]+" (Arbitrary "Name") Pop
    , tokNext "[A-Z_]+(?=[^a-zA-Z0-9_])" (Arbitrary "Name" :. Arbitrary "Constant") Pop
    , tokNext "(?=[^a-zA-Z0-9_])" (Arbitrary "Text") Pop
    ]

varname' :: TokenMatcher
varname' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "\\{" (Arbitrary "Punctuation") Pop
    , tokNext "\\)|," (Arbitrary "Punctuation") Pop
    , tok "[a-zA-Z0-9_]+::" (Arbitrary "Name" :. Arbitrary "Namespace")
    , tokNext "[a-zA-Z0-9_:]+" (Arbitrary "Name" :. Arbitrary "Variable") Pop
    ]

endPart' :: TokenMatcher
endPart' =
    [ tokNext ".+" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    ]

sbString' :: TokenMatcher
sbString' =
    [ tok "\\\\[\\[\\]\\\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tokNext "\\[" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo sbString')
    , tokNext "\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") Pop
    , tok "[^\\[\\]]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    ]

funcname' :: TokenMatcher
funcname' =
    [ tok "[a-zA-Z_][\\w_]*[\\!\\?]?" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "\\s+" (Arbitrary "Text")
    , tok "(\\([$@%]*\\))(\\s*)" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Text")])
    , tokNext ".*?{" (Arbitrary "Punctuation") Pop
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "\\#.*?$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "^=[a-zA-Z0-9]+\\s+.*?\\n=cut" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "(case|continue|do|else|elsif|for|foreach|if|last|my|next|our|redo|reset|then|unless|until|while|use|print|new|BEGIN|CHECK|INIT|END|return)\\b" (Arbitrary "Keyword")
    , tokNext "(format)(\\s+)([a-zA-Z0-9_]+)(\\s*)(=)(\\s*\\n)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Text")]) (GoTo format')
    , tok "(eq|lt|gt|le|ge|ne|not|and|or|cmp)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "s/(\\\\\\\\|\\\\/|[^/])*/(\\\\\\\\|\\\\/|[^/])*/[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "s!(\\\\\\\\|\\\\!|[^!])*!(\\\\\\\\|\\\\!|[^!])*![egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "s\\\\(\\\\\\\\|[^\\\\])*\\\\(\\\\\\\\|[^\\\\])*\\\\[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "s@(\\\\\\\\|\\\\@|[^@])*@(\\\\\\\\|\\\\@|[^@])*@[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "s%(\\\\\\\\|\\\\%|[^%])*%(\\\\\\\\|\\\\%|[^%])*%[egimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tokNext "s{(\\\\\\\\|\\\\}|[^}])*}\\s*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") (GoTo balancedRegex')
    , tokNext "s<(\\\\\\\\|\\\\>|[^>])*>\\s*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") (GoTo balancedRegex')
    , tokNext "s\\[(\\\\\\\\|\\\\\\]|[^\\]])*\\]\\s*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") (GoTo balancedRegex')
    , tokNext "s\\((\\\\\\\\|\\\\\\)|[^\\)])*\\)\\s*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") (GoTo balancedRegex')
    , tok "m?/(\\\\\\\\|\\\\/|[^/\\n])*/[gcimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tokNext "m(?=[/!\\\\{<\\[\\(@%\\$])" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex") (GoTo balancedRegex')
    , tok "((?<==\126)|(?<=\\())\\s*/(\\\\\\\\|\\\\/|[^/])*/[gcimosx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "\\s+" (Arbitrary "Text")
    , tok "(abs|accept|alarm|atan2|bind|binmode|bless|caller|chdir|chmod|chomp|chop|chown|chr|chroot|close|closedir|connect|continue|cos|crypt|dbmclose|dbmopen|defined|delete|die|dump|each|endgrent|endhostent|endnetent|endprotoent|endpwent|endservent|eof|eval|exec|exists|exit|exp|fcntl|fileno|flock|fork|format|formline|getc|getgrent|getgrgid|getgrnam|gethostbyaddr|gethostbyname|gethostent|getlogin|getnetbyaddr|getnetbyname|getnetent|getpeername|getpgrp|getppid|getpriority|getprotobyname|getprotobynumber|getprotoent|getpwent|getpwnam|getpwuid|getservbyname|getservbyport|getservent|getsockname|getsockopt|glob|gmtime|goto|grep|hex|import|index|int|ioctl|join|keys|kill|last|lc|lcfirst|length|link|listen|local|localtime|log|lstat|map|mkdir|msgctl|msgget|msgrcv|msgsnd|my|next|no|oct|open|opendir|ord|our|pack|package|pipe|pop|pos|printf|prototype|push|quotemeta|rand|read|readdir|readline|readlink|readpipe|recv|redo|ref|rename|require|reverse|rewinddir|rindex|rmdir|scalar|seek|seekdir|select|semctl|semget|semop|send|setgrent|sethostent|setnetent|setpgrp|setpriority|setprotoent|setpwent|setservent|setsockopt|shift|shmctl|shmget|shmread|shmwrite|shutdown|sin|sleep|socket|socketpair|sort|splice|split|sprintf|sqrt|srand|stat|study|substr|symlink|syscall|sysopen|sysread|sysseek|system|syswrite|tell|telldir|tie|tied|time|times|tr|truncate|uc|ucfirst|umask|undef|unlink|unpack|unshift|untie|utime|values|vec|wait|waitpid|wantarray|warn|write)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "((__(DATA|DIE|WARN)__)|(STD(IN|OUT|ERR)))\\b" (Arbitrary "Name" :. Arbitrary "Builtin" :. Arbitrary "Pseudo")
    , tok "<<([\\'\"]?)([a-zA-Z_][a-zA-Z0-9_]*)\\1;?\\n.*?\\n\\2\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "__END__" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo endPart')
    , tok "\\$\\^[ADEFHILMOPSTWX]" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Global")
    , tok "\\$[\\\\\\\"\\[\\]'&`+*.,;=%\126?@$!<>(^|/-](?!\\w)" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Global")
    , tokNext "[$@%#]+" (Arbitrary "Name" :. Arbitrary "Variable") (GoTo varname')
    , tok "0_?[0-7]+(_[0-7]+)*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0x[0-9A-Fa-f]+(_[0-9A-Fa-f]+)*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0b[01]+(_[01]+)*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Bin")
    , tok "(?i)(\\d*(_\\d*)*\\.\\d+(_\\d*)*|\\d+(_\\d*)*\\.\\d+(_\\d*)*)(e[+-]?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(?i)\\d+(_\\d*)*e[+-]?\\d+(_\\d*)*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+(_\\d+)*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "`(\\\\\\\\|\\\\`|[^`])*`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick")
    , tok "<([^\\s>]+)>" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tokNext "(q|qq|qw|qr|qx)\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo cbString')
    , tokNext "(q|qq|qw|qr|qx)\\(" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo rbString')
    , tokNext "(q|qq|qw|qr|qx)\\[" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo sbString')
    , tokNext "(q|qq|qw|qr|qx)\\<" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo ltString')
    , tok "(q|qq|qw|qr|qx)([^a-zA-Z0-9])(.|\\n)*?\\2" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tokNext "package\\s+" (Arbitrary "Keyword") (GoTo modulename')
    , tokNext "sub\\s+" (Arbitrary "Keyword") (GoTo funcname')
    , tok "(\\[\\]|\\*\\*|::|<<|>>|>=|<=|<=>|={3}|!=|=\126|!\126|&&?|\\|\\||\\.{1,3})" (Arbitrary "Operator")
    , tok "[-+/*%=<>&^|!\\\\\126]=?" (Arbitrary "Operator")
    , tok "[\\(\\)\\[\\]:;,<>/\\?\\{\\}]" (Arbitrary "Punctuation")
    , tokNext "(?=\\w)" (Arbitrary "Name") (GoTo name')
    ]

cbString' :: TokenMatcher
cbString' =
    [ tok "\\\\[\\{\\}\\\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tokNext "\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo cbString')
    , tokNext "\\}" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") Pop
    , tok "[^\\{\\}\\\\]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    ]

ltString' :: TokenMatcher
ltString' =
    [ tok "\\\\[\\<\\>\\\\]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tokNext "\\<" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo ltString')
    , tokNext "\\>" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") Pop
    , tok "[^\\<\\>]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    ]

