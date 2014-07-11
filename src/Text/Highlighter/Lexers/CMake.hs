module Text.Highlighter.Lexers.CMake (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "CMake"
    , lAliases = ["cmake"]
    , lExtensions = [".cmake", "CMakeLists.txt"]
    , lMimetypes = ["text/x-cmake"]
    , lStart = root'
    , lFlags = [multiline]
    }

keywords' :: TokenMatcher
keywords' =
    [ tok "\\b(WIN32|UNIX|APPLE|CYGWIN|BORLAND|MINGW|MSVC|MSVC_IDE|MSVC60|MSVC70|MSVC71|MSVC80|MSVC90)\\b" (Arbitrary "Keyword")
    ]

ws' :: TokenMatcher
ws' =
    [ tok "[ \\t]+" (Arbitrary "Text")
    , tok "#.+\\n" (Arbitrary "Comment")
    ]

args' :: TokenMatcher
args' =
    [ tokNext "\\(" (Arbitrary "Punctuation") Push
    , tokNext "\\)" (Arbitrary "Punctuation") Pop
    , tok "(\\${)(.+?)(})" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Operator")])
    , tok "(?s)\".*?\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "\\\\\\S+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "[^\\)$\"# \\t\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\n" (Arbitrary "Text")
    , anyOf keywords'
    , anyOf ws'
    ]

root' :: TokenMatcher
root' =
    [ tokNext "\\b([A-Za-z_]+)([ \\t]*)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Text"), (Arbitrary "Punctuation")]) (GoTo args')
    , anyOf keywords'
    , anyOf ws'
    ]

string' :: TokenMatcher
string' =
    [ 
    ]

