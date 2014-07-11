module Text.Highlighter.Lexers.MuPAD (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "MuPAD"
    , lAliases = ["mupad"]
    , lExtensions = [".mu"]
    , lMimetypes = []
    , lStart = root'
    , lFlags = [multiline]
    }

comment' :: TokenMatcher
comment' =
    [ tok "[^*/]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "/\\*" (Arbitrary "Comment" :. Arbitrary "Multiline") Push
    , tokNext "\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tok "[*/]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

root' :: TokenMatcher
root' =
    [ tok "//.*?$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "/\\*" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo comment')
    , tok "\"(?:[^\"\\\\]|\\\\.)*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\(|\\)|\\[|\\]|\\{|\\}" (Arbitrary "Punctuation")
    , tok "(?x)\\b(?:\10            next|break|end|\10            axiom|end_axiom|category|end_category|domain|end_domain|inherits|\10            if|%if|then|elif|else|end_if|\10            case|of|do|otherwise|end_case|\10            while|end_while|\10            repeat|until|end_repeat|\10            for|from|to|downto|step|end_for|\10            proc|local|option|save|begin|end_proc|\10            delete|frame\10          )\\b" (Arbitrary "Keyword")
    , tok "(?x)\\b(?:\10            DOM_ARRAY|DOM_BOOL|DOM_COMPLEX|DOM_DOMAIN|DOM_EXEC|DOM_EXPR|\10            DOM_FAIL|DOM_FLOAT|DOM_FRAME|DOM_FUNC_ENV|DOM_HFARRAY|DOM_IDENT|\10            DOM_INT|DOM_INTERVAL|DOM_LIST|DOM_NIL|DOM_NULL|DOM_POLY|DOM_PROC|\10            DOM_PROC_ENV|DOM_RAT|DOM_SET|DOM_STRING|DOM_TABLE|DOM_VAR\10          )\\b" (Arbitrary "Name" :. Arbitrary "Class")
    , tok "(?x)\\b(?:\10            PI|EULER|E|CATALAN|\10            NIL|FAIL|undefined|infinity|\10            TRUE|FALSE|UNKNOWN\10          )\\b" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "\\b(?:dom|procname)\\b" (Arbitrary "Name" :. Arbitrary "Builtin" :. Arbitrary "Pseudo")
    , tok "\\.|,|:|;|=|\\+|-|\\*|/|\\^|@|>|<|\\$|\\||!|\\'|%|\126=" (Arbitrary "Operator")
    , tok "(?x)\\b(?:\10            and|or|not|xor|\10            assuming|\10            div|mod|\10            union|minus|intersect|in|subset\10          )\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tok "\\b(?:I|RDN_INF|RD_NINF|RD_NAN)\\b" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "(?x)\10          ((?:[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)\10          (?:::[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)*)\\s*([(])" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Punctuation")])
    , tok "(?x)\10          (?:[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)\10          (?:::[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[0-9]+(?:\\.[0-9]*)?(?:e[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\.[0-9]+(?:e[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "." (Arbitrary "Text")
    ]

