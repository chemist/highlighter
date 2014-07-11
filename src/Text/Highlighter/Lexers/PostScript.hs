module Text.Highlighter.Lexers.PostScript (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "PostScript"
    , lAliases = ["postscript"]
    , lExtensions = [".ps", ".eps"]
    , lMimetypes = ["application/postscript"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "^%!.+\\n" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "%%.*\\n" (Arbitrary "Comment" :. Arbitrary "Special")
    , tok "(^%.*\\n){2,}" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "%.*\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "\\(" (Arbitrary "Literal" :. Arbitrary "String") (GoTo stringliteral')
    , tok "[\\{\\}(\\<\\<)(\\>\\>)\\[\\]]" (Arbitrary "Punctuation")
    , tok "<[0-9A-Fa-f]+>(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9]+\\#(\\-|\\+)?([0-9]+\\.?|[0-9]*\\.[0-9]+|[0-9]+\\.[0-9]*)((e|E)[0-9]+)?(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "(\\-|\\+)?([0-9]+\\.?|[0-9]*\\.[0-9]+|[0-9]+\\.[0-9]*)((e|E)[0-9]+)?(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\-|\\+)?[0-9]+(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\/[^\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s]+(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[^\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s]+(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "(false|true)(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(eq|ne|ge|gt|le|lt|and|or|not|if|ifelse|for|forall)(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    , tok "(abs|add|aload|arc|arcn|array|atan|begin|bind|ceiling|charpath|clip|closepath|concat|concatmatrix|copy|cos|currentlinewidth|currentmatrix|currentpoint|curveto|cvi|cvs|def|defaultmatrix|dict|dictstackoverflow|div|dtransform|dup|end|exch|exec|exit|exp|fill|findfont|floor|get|getinterval|grestore|gsave|gt|identmatrix|idiv|idtransform|index|invertmatrix|itransform|length|lineto|ln|load|log|loop|matrix|mod|moveto|mul|neg|newpath|pathforall|pathbbox|pop|print|pstack|put|quit|rand|rangecheck|rcurveto|repeat|restore|rlineto|rmoveto|roll|rotate|round|run|save|scale|scalefont|setdash|setfont|setgray|setlinecap|setlinejoin|setlinewidth|setmatrix|setrgbcolor|shfill|show|showpage|sin|sqrt|stack|stringwidth|stroke|strokepath|sub|syntaxerror|transform|translate|truncate|typecheck|undefined|undefinedfilename|undefinedresult)(?=[\\(\\)\\<\\>\\[\\]\\{\\}\\/\\%\\s])" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "\\s+" (Arbitrary "Text")
    ]

stringliteral' :: TokenMatcher
stringliteral' =
    [ tok "[^\\(\\)\\\\]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "\\\\" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") (GoTo escape')
    , tokNext "\\(" (Arbitrary "Literal" :. Arbitrary "String") Push
    , tokNext "\\)" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

escape' :: TokenMatcher
escape' =
    [ tokNext "([0-8]{3}|n|r|t|b|f|\\\\|\\(|\\)|)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape") Pop
    ]

