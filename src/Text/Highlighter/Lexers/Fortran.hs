module Text.Highlighter.Lexers.Fortran (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Fortran"
    , lAliases = ["fortran"]
    , lExtensions = [".f", ".f90"]
    , lMimetypes = ["text/x-fortran"]
    , lStart = root'
    , lFlags = [caseless]
    }

core' :: TokenMatcher
core' =
    [ tok "\\b(ACCEPT|ALLOCATABLE|ALLOCATE|ARRAY|ASSIGN|BACKSPACE|BLOCK DATA|BYTE|CALL|CASE|CLOSE|COMMON|CONTAINS|CONTINUE|CYCLE|DATA|DEALLOCATE|DECODE|DIMENSION|DO|ENCODE|END FILE|ENDIF|END|ENTRY|EQUIVALENCE|EXIT|EXTERNAL|EXTRINSIC|FORALL|FORMAT|FUNCTION|GOTO|IF|IMPLICIT|INCLUDE|INQUIRE|INTENT|INTERFACE|INTRINSIC|MODULE|NAMELIST|NULLIFY|NONE|OPEN|OPTIONAL|OPTIONS|PARAMETER|PAUSE|POINTER|PRINT|PRIVATE|PROGRAM|PUBLIC|PURE|READ|RECURSIVE|RETURN|REWIND|SAVE|SELECT|SEQUENCE|STOP|SUBROUTINE|TARGET|TYPE|USE|VOLATILE|WHERE|WRITE|WHILE|THEN|ELSE|ENDIF)\\s*\\b" (Arbitrary "Keyword")
    , tok "\\b(CHARACTER|COMPLEX|DOUBLE PRECISION|DOUBLE COMPLEX|INTEGER|LOGICAL|REAL)\\s*\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(\\*\\*|\\*|\\+|-|\\/|<|>|<=|>=|==|\\/=|=)" (Arbitrary "Operator")
    , tok "(::)" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "[(),:&%;]" (Arbitrary "Punctuation")
    , tok "\\b(Abort|Abs|Access|AChar|ACos|AdjustL|AdjustR|AImag|AInt|Alarm|All|Allocated|ALog|AMax|AMin|AMod|And|ANInt|Any|ASin|Associated|ATan|BesJ|BesJN|BesY|BesYN|Bit_Size|BTest|CAbs|CCos|Ceiling|CExp|Char|ChDir|ChMod|CLog|Cmplx|Complex|Conjg|Cos|CosH|Count|CPU_Time|CShift|CSin|CSqRt|CTime|DAbs|DACos|DASin|DATan|Date_and_Time|DbesJ|DbesJ|DbesJN|DbesY|DbesY|DbesYN|Dble|DCos|DCosH|DDiM|DErF|DErFC|DExp|Digits|DiM|DInt|DLog|DLog|DMax|DMin|DMod|DNInt|Dot_Product|DProd|DSign|DSinH|DSin|DSqRt|DTanH|DTan|DTime|EOShift|Epsilon|ErF|ErFC|ETime|Exit|Exp|Exponent|FDate|FGet|FGetC|Float|Floor|Flush|FNum|FPutC|FPut|Fraction|FSeek|FStat|FTell|GError|GetArg|GetCWD|GetEnv|GetGId|GetLog|GetPId|GetUId|GMTime|HostNm|Huge|IAbs|IAChar|IAnd|IArgC|IBClr|IBits|IBSet|IChar|IDate|IDiM|IDInt|IDNInt|IEOr|IErrNo|IFix|Imag|ImagPart|Index|Int|IOr|IRand|IsaTty|IShft|IShftC|ISign|ITime|Kill|Kind|LBound|Len|Len_Trim|LGe|LGt|Link|LLe|LLt|LnBlnk|Loc|Log|Log|Logical|Long|LShift|LStat|LTime|MatMul|Max|MaxExponent|MaxLoc|MaxVal|MClock|Merge|Min|MinExponent|MinLoc|MinVal|Mod|Modulo|MvBits|Nearest|NInt|Not|Or|Pack|PError|Precision|Present|Product|Radix|Rand|Random_Number|Random_Seed|Range|Real|RealPart|Rename|Repeat|Reshape|RRSpacing|RShift|Scale|Scan|Second|Selected_Int_Kind|Selected_Real_Kind|Set_Exponent|Shape|Short|Sign|Signal|SinH|Sin|Sleep|Sngl|Spacing|Spread|SqRt|SRand|Stat|Sum|SymLnk|System|System_Clock|Tan|TanH|Time|Tiny|Transfer|Transpose|Trim|TtyNam|UBound|UMask|Unlink|Unpack|Verify|XOr|ZAbs|ZCos|ZExp|ZLog|ZSin|ZSqRt)\\s*\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "\\.(true|false)\\." (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "\\.(eq|ne|lt|le|gt|ge|not|and|or|eqv|neqv)\\." (Arbitrary "Operator" :. Arbitrary "Word")
    ]

root' :: TokenMatcher
root' =
    [ tok "!.*\\n" (Arbitrary "Comment")
    , anyOf strings'
    , anyOf core'
    , tok "[a-z][a-z0-9_]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , anyOf nums'
    , tok "[\\s]+" (Arbitrary "Text")
    ]

strings' :: TokenMatcher
strings' =
    [ tok "(?s)\"(\\\\\\\\|\\\\[0-7]+|\\\\.|[^\"\\\\])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "(?s)'(\\\\\\\\|\\\\[0-7]+|\\\\.|[^'\\\\])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    ]

nums' :: TokenMatcher
nums' =
    [ tok "\\d+(?![.Ee])" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[+-]?\\d*\\.\\d+([eE][-+]?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[+-]?\\d+\\.\\d*([eE][-+]?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    ]

