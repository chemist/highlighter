module Text.Highlighter.Lexers.Scheme (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Scheme"
    , lAliases = ["scheme", "scm"]
    , lExtensions = [".scm"]
    , lMimetypes = ["text/x-scheme", "application/x-scheme"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok ";.*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "\\s+" (Arbitrary "Text")
    , tok "-?\\d+\\.\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "-?\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "'[a-zA-Z0-9!$%&*+,/:<=>?@^_\126|-]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "#\\\\([()/'\\\".'_!\194\167$%& ?=+-]{1}|[a-zA-Z0-9]+)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(#t|#f)" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "('|#|`|,@|,|\\.)" (Arbitrary "Operator")
    , tok "(lambda |define |if |else |cond |and |or |case |let |let\\* |letrec |begin |do |delay |set\\! |\\=\\> |quote |quasiquote |unquote |unquote\\-splicing |define\\-syntax |let\\-syntax |letrec\\-syntax |syntax\\-rules )" (Arbitrary "Keyword")
    , tok "(?<='\\()[a-zA-Z0-9!$%&*+,/:<=>?@^_\126|-]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(?<=#\\()[a-zA-Z0-9!$%&*+,/:<=>?@^_\126|-]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(?<=\\()(\\* |\\+ |\\- |\\/ |\\< |\\<\\= |\\= |\\> |\\>\\= |abs |acos |angle |append |apply |asin |assoc |assq |assv |atan |boolean\\? |caaaar |caaadr |caaar |caadar |caaddr |caadr |caar |cadaar |cadadr |cadar |caddar |cadddr |caddr |cadr |call\\-with\\-current\\-continuation |call\\-with\\-input\\-file |call\\-with\\-output\\-file |call\\-with\\-values |call\\/cc |car |cdaaar |cdaadr |cdaar |cdadar |cdaddr |cdadr |cdar |cddaar |cddadr |cddar |cdddar |cddddr |cdddr |cddr |cdr |ceiling |char\\-\\>integer |char\\-alphabetic\\? |char\\-ci\\<\\=\\? |char\\-ci\\<\\? |char\\-ci\\=\\? |char\\-ci\\>\\=\\? |char\\-ci\\>\\? |char\\-downcase |char\\-lower\\-case\\? |char\\-numeric\\? |char\\-ready\\? |char\\-upcase |char\\-upper\\-case\\? |char\\-whitespace\\? |char\\<\\=\\? |char\\<\\? |char\\=\\? |char\\>\\=\\? |char\\>\\? |char\\? |close\\-input\\-port |close\\-output\\-port |complex\\? |cons |cos |current\\-input\\-port |current\\-output\\-port |denominator |display |dynamic\\-wind |eof\\-object\\? |eq\\? |equal\\? |eqv\\? |eval |even\\? |exact\\-\\>inexact |exact\\? |exp |expt |floor |for\\-each |force |gcd |imag\\-part |inexact\\-\\>exact |inexact\\? |input\\-port\\? |integer\\-\\>char |integer\\? |interaction\\-environment |lcm |length |list |list\\-\\>string |list\\-\\>vector |list\\-ref |list\\-tail |list\\? |load |log |magnitude |make\\-polar |make\\-rectangular |make\\-string |make\\-vector |map |max |member |memq |memv |min |modulo |negative\\? |newline |not |null\\-environment |null\\? |number\\-\\>string |number\\? |numerator |odd\\? |open\\-input\\-file |open\\-output\\-file |output\\-port\\? |pair\\? |peek\\-char |port\\? |positive\\? |procedure\\? |quotient |rational\\? |rationalize |read |read\\-char |real\\-part |real\\? |remainder |reverse |round |scheme\\-report\\-environment |set\\-car\\! |set\\-cdr\\! |sin |sqrt |string |string\\-\\>list |string\\-\\>number |string\\-\\>symbol |string\\-append |string\\-ci\\<\\=\\? |string\\-ci\\<\\? |string\\-ci\\=\\? |string\\-ci\\>\\=\\? |string\\-ci\\>\\? |string\\-copy |string\\-fill\\! |string\\-length |string\\-ref |string\\-set\\! |string\\<\\=\\? |string\\<\\? |string\\=\\? |string\\>\\=\\? |string\\>\\? |string\\? |substring |symbol\\-\\>string |symbol\\? |tan |transcript\\-off |transcript\\-on |truncate |values |vector |vector\\-\\>list |vector\\-fill\\! |vector\\-length |vector\\-ref |vector\\-set\\! |vector\\? |with\\-input\\-from\\-file |with\\-output\\-to\\-file |write |write\\-char |zero\\? )" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(?<=\\()[a-zA-Z0-9!$%&*+,/:<=>?@^_\126|-]+" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "[a-zA-Z0-9!$%&*+,/:<=>?@^_\126|-]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(\\(|\\))" (Arbitrary "Punctuation")
    ]

