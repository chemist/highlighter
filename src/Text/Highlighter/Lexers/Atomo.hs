module Text.Highlighter.Lexers.Atomo (lexer) where

import Data.List (intercalate)
import Text.Printf
import Text.Regex.PCRE.Light

import Text.Highlighter.Types


lexer :: Lexer
lexer = Lexer
    { lName = "Atomo"
    , lAliases = ["atomo"]
    , lExtensions = [".atomo"]
    , lMimetypes = ["text/x-atomo"]
    , lStart = root
    , lFlags = [multiline]
    }

ascii :: [String]
ascii =
    ["NUL","SOH","[SE]TX","EOT","ENQ","ACK",
     "BEL","BS","HT","LF","VT","FF","CR","S[OI]","DLE",
     "DC[1-4]","NAK","SYN","ETB","CAN",
     "EM","SUB","ESC","[FGRU]S","SP","DEL"]

reserved :: [String]
reserved = ["operator", "macro", "for-macro", "this"]

identifier :: String
identifier = "[a-zA-Z0-9_!#%&\\*\\+\\.\\\\/<=>\\?@^\\|~\\-]"

operator :: String
operator = "[:!#%&\\*\\+\\.\\\\/<=>\\?@^\\|~\\-]"

root :: TokenMatcher
root =
    -- Comments
    [ tok "--.*?$" (Comment :. Single)
    , tokNext "{-" (Comment :. Multiline) (GoTo comment)

    -- Boolean
    , tok "True|False" (Keyword :. Constant)

    -- Numbers
    , tok "[\\+\\-]?\\d+[eE][\\+\\-]?\\d+" (Number :. Float)
    , tok "[\\+\\-]?\\d+\\.\\d+([eE][\\+\\-]?\\d+)?" (Number :. Float)
    , tok "[\\+\\-]?0[oO][0-7]+" (Number :. Oct)
    , tok "[\\+\\-]?0[xX][\\da-fA-F]+" (Number :. Hex)
    , tok "[\\+\\-]?\\d+/[\\+\\-]?\\d+" Number
    , tok "[\\+\\-]?\\d+" (Number :. Integer)

    -- Internal representations (TODO: these should get less ambiguous syntax.)
    , tokNext "<[a-z]" (Generic :. Output) (GoTo internal)

    -- Macro-Quote
    , tokNext ("(?![\"$|`;~@])(" ++ identifier ++ "+)([\"$|`'~@])") (String :. Other) (CapturesTo macroQuote)
    , tokNext ("(?![\"$|`;~@])(" ++ identifier ++ "+)\\(") (String :. Other) (GoTo (macroQuoteDelim "\\)"))
    , tokNext ("(?![\"$|`;~@])(" ++ identifier ++ "+)\\{") (String :. Other) (GoTo (macroQuoteDelim "\\}"))
    , tokNext ("(?![\"$|`;~@])(" ++ identifier ++ "+)\\[") (String :. Other) (GoTo (macroQuoteDelim "\\]"))

    -- Identifiers
    , tok (printf "\\b(%s)\\b(?!%s)" (intercalate "|" reserved) operator)
        (Keyword :. Reserved)
    , tok ("(?![@$~])(?!" ++ operator ++ "+(\\s|$))" ++ identifier ++ "+:") (Name :. Function)
    {-, tok ("[A-Z]" ++ identifier ++ "*") (Name :. Variable :. Global)-}
    , tok ("(?![@$~])(?!" ++ operator ++ "+(\\s|$))" ++ identifier ++ "+") Name

    -- Operators
    , tok ("(?![@$~])" ++ operator ++ "+") Operator

    -- Whitespace
    , tok "\\s+" Text

    -- Characters & Strings
    , tokNext "\\$" (String :. Char) (GoTo character)
    , tokNext "\"" String (GoTo string)

    -- Quoting
    , tok ("'" ++ identifier ++ "+") (String :. Symbol)
    , tok "'" (String :. Symbol)
    , tok ("`" ++ identifier ++ "+") (String :. Symbol)
    , tok "`" (String :. Symbol)
    , tok ("~" ++ identifier ++ "+") (String :. Interpol)
    , tok "~" (String :. Interpol)

    -- Particles
    , tok ("@(" ++ identifier ++ "+:)+") (Name :. Decorator)
    , tok ("@" ++ identifier ++ "+") (Name :. Decorator)
    , tok "@" (Name :. Decorator)

    -- Punctuation
    , tok "[][(),;{}|]" Punctuation
    ]

internal :: TokenMatcher
internal =
    [ tok "[^<>]+" (Generic :. Output)
    , tokNext "<" (Generic :. Output) Push
    , tokNext ">" (Generic :. Output) Pop
    ]

comment :: TokenMatcher
comment =
    [ tok "[^\\-\\{\\}]+" (Comment :. Multiline)
    , tokNext "{-" (Comment :. Multiline) Push
    , tokNext "-}" (Comment :. Multiline) Pop
    , tok "[-{}]" (Comment :. Multiline)
    ]

character :: TokenMatcher
character =
    [ tokNext "[^\\\\]" (String :. Char) Pop
    , tokNext "\\\\[^\\s]+" (String :. Escape) Pop
    ]

string :: TokenMatcher
string =
    [ tok "[^\\\\\"]+" String
    , tokNext "\\\\" (String :. Escape) (GoTo escape)
    , tokNext "\"" String Pop
    ]

macroQuoteDelim :: String -> TokenMatcher
macroQuoteDelim c =
    [ tok ("[^\\\\" ++ c ++ "]+") (String :. Other)
    , tokNext "\\\\." (String :. Other) Continue
    , tokNext (c ++ "([[:alpha:]]*)") (String :. Other) Pop
    ]

macroQuote :: [String] -> TokenMatcher
macroQuote cs =
    [ tok ("[^\\\\" ++ (cs !! 2) ++ "]+") (String :. Other)
    , tokNext "\\\\." (String :. Other) Continue
    , tokNext ((cs !! 2) ++ "([[:alpha:]]*)") (String :. Other) Pop
    ]

escape :: TokenMatcher
escape =
    [ tokNext "[abfnrtv\"&\\\\]" (String :. Escape) Pop
    , tokNext "\\^[\\]\\[A-Z@\\^_]" (String :. Escape) Pop
    , tokNext (intercalate "|" ascii) (String :. Escape) Pop
    , tokNext "o[0-7]+" (String :. Escape) Pop
    , tokNext "x[\\da-fA-F]+" (String :. Escape) Pop
    , tokNext "\\d+" (String :. Escape) Pop
    , tokNext "\\s+\\\\" (String :. Escape) Pop
    ]
