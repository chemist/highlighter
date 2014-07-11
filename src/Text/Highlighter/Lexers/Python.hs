module Text.Highlighter.Lexers.Python (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Python"
    , lAliases = ["python", "py"]
    , lExtensions = [".py", ".pyw", ".sc", "SConstruct", "SConscript", ".tac"]
    , lMimetypes = ["text/x-python", "application/x-python"]
    , lStart = root'
    , lFlags = [multiline]
    }

tsqs' :: TokenMatcher
tsqs' =
    [ tokNext "'''" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf strings'
    , anyOf nl'
    ]

builtins' :: TokenMatcher
builtins' =
    [ tok "(?<!\\.)(__import__|abs|all|any|apply|basestring|bin|bool|buffer|bytearray|bytes|callable|chr|classmethod|cmp|coerce|compile|complex|delattr|dict|dir|divmod|enumerate|eval|execfile|exit|file|filter|float|frozenset|getattr|globals|hasattr|hash|hex|id|input|int|intern|isinstance|issubclass|iter|len|list|locals|long|map|max|min|next|object|oct|open|ord|pow|property|range|raw_input|reduce|reload|repr|reversed|round|set|setattr|slice|sorted|staticmethod|str|sum|super|tuple|type|unichr|unicode|vars|xrange|zip)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(?<!\\.)(self|None|Ellipsis|NotImplemented|False|True)\\b" (Arbitrary "Name" :. Arbitrary "Builtin" :. Arbitrary "Pseudo")
    , tok "(?<!\\.)(ArithmeticError|AssertionError|AttributeError|BaseException|DeprecationWarning|EOFError|EnvironmentError|Exception|FloatingPointError|FutureWarning|GeneratorExit|IOError|ImportError|ImportWarning|IndentationError|IndexError|KeyError|KeyboardInterrupt|LookupError|MemoryError|NameError|NotImplemented|NotImplementedError|OSError|OverflowError|OverflowWarning|PendingDeprecationWarning|ReferenceError|RuntimeError|RuntimeWarning|StandardError|StopIteration|SyntaxError|SyntaxWarning|SystemError|SystemExit|TabError|TypeError|UnboundLocalError|UnicodeDecodeError|UnicodeEncodeError|UnicodeError|UnicodeTranslateError|UnicodeWarning|UserWarning|ValueError|VMSError|Warning|WindowsError|ZeroDivisionError)\\b" (Arbitrary "Name" :. Arbitrary "Exception")
    ]

name' :: TokenMatcher
name' =
    [ tok "@[a-zA-Z0-9_.]+" (Arbitrary "Name" :. Arbitrary "Decorator")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    ]

dqs' :: TokenMatcher
dqs' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\\\\\|\\\\\"|\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , anyOf strings'
    ]

sqs' :: TokenMatcher
sqs' =
    [ tokNext "'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\\\\\|\\\\'|\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , anyOf strings'
    ]

tdqs' :: TokenMatcher
tdqs' =
    [ tokNext "\"\"\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf strings'
    , anyOf nl'
    ]

backtick' :: TokenMatcher
backtick' =
    [ tok "`.*?`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Backtick")
    ]

funcname' :: TokenMatcher
funcname' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Function") Pop
    ]

classname' :: TokenMatcher
classname' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

stringescape' :: TokenMatcher
stringescape' =
    [ tok "\\\\([\\\\abfnrtv\"\\']|\\n|N{.*?}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    ]

numbers' :: TokenMatcher
numbers' =
    [ tok "(\\d+\\.\\d*|\\d*\\.\\d+)([eE][+-]?[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+[eE][+-]?[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[0-7]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[xX][a-fA-F0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "\\d+L" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer" :. Arbitrary "Long")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    ]

keywords' :: TokenMatcher
keywords' =
    [ tok "(assert|break|continue|del|elif|else|except|exec|finally|for|global|if|lambda|pass|print|raise|return|try|while|yield|as|with)\\b" (Arbitrary "Keyword")
    ]

import' :: TokenMatcher
import' =
    [ tok "((?:\\s|\\\\\\s)+)(as)((?:\\s|\\\\\\s)+)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")])
    , tok "[a-zA-Z_][a-zA-Z0-9_.]*" (Arbitrary "Name" :. Arbitrary "Namespace")
    , tok "(\\s*)(,)(\\s*)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Operator"), (Arbitrary "Text")])
    , tokNext "" (Arbitrary "Text") Pop
    ]

nl' :: TokenMatcher
nl' =
    [ tok "\\n" (Arbitrary "Literal" :. Arbitrary "String")
    ]

root' :: TokenMatcher
root' =
    [ tok "\\n" (Arbitrary "Text")
    , tok "^(\\s*)([rRuU]{,2}\"\"\"(?:.|\\n)*?\"\"\")" (ByGroups [(Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Doc")])
    , tok "^(\\s*)([rRuU]{,2}'''(?:.|\\n)*?''')" (ByGroups [(Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Doc")])
    , tok "[^\\S\\n]+" (Arbitrary "Text")
    , tok "#.*$" (Arbitrary "Comment")
    , tok "[]{}:(),;[]" (Arbitrary "Punctuation")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "\\\\" (Arbitrary "Text")
    , tok "(in|is|and|or|not)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "!=|==|<<|>>|[-\126+/*%=<>&^|.]" (Arbitrary "Operator")
    , anyOf keywords'
    , tokNext "(def)((?:\\s|\\\\\\s)+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo funcname')
    , tokNext "(class)((?:\\s|\\\\\\s)+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tokNext "(from)((?:\\s|\\\\\\s)+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")]) (GoTo fromimport')
    , tokNext "(import)((?:\\s|\\\\\\s)+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")]) (GoTo import')
    , anyOf builtins'
    , anyOf backtick'
    , tokNext "(?:[rR]|[uU][rR]|[rR][uU])\"\"\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo tdqs')
    , tokNext "(?:[rR]|[uU][rR]|[rR][uU])'''" (Arbitrary "Literal" :. Arbitrary "String") (GoTo tsqs')
    , tokNext "(?:[rR]|[uU][rR]|[rR][uU])\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo dqs')
    , tokNext "(?:[rR]|[uU][rR]|[rR][uU])'" (Arbitrary "Literal" :. Arbitrary "String") (GoTo sqs')
    , tokNext "[uU]?\"\"\"" (Arbitrary "Literal" :. Arbitrary "String") (Combined [stringescape', tdqs'])
    , tokNext "[uU]?'''" (Arbitrary "Literal" :. Arbitrary "String") (Combined [stringescape', tsqs'])
    , tokNext "[uU]?\"" (Arbitrary "Literal" :. Arbitrary "String") (Combined [stringescape', dqs'])
    , tokNext "[uU]?'" (Arbitrary "Literal" :. Arbitrary "String") (Combined [stringescape', sqs'])
    , anyOf name'
    , anyOf numbers'
    ]

strings' :: TokenMatcher
strings' =
    [ tok "%(\\([a-zA-Z0-9_]+\\))?[-#0 +]*([0-9]+|[*])?(\\.([0-9]+|[*]))?[hlL]?[diouxXeEfFgGcrs%]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol")
    , tok "[^\\\\\\'\"%\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "[\\'\"\\\\]" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "%" (Arbitrary "Literal" :. Arbitrary "String")
    ]

fromimport' :: TokenMatcher
fromimport' =
    [ tokNext "((?:\\s|\\\\\\s)+)(import)\\b" (ByGroups [(Arbitrary "Text"), (Arbitrary "Keyword" :. Arbitrary "Namespace")]) Pop
    , tok "[a-zA-Z_.][a-zA-Z0-9_.]*" (Arbitrary "Name" :. Arbitrary "Namespace")
    ]

