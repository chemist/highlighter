module Text.Highlighter.Lexers.Logtalk (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Logtalk"
    , lAliases = ["logtalk"]
    , lExtensions = [".lgt"]
    , lMimetypes = ["text/x-logtalk"]
    , lStart = root'
    , lFlags = [multiline]
    }

quoted_atom' :: TokenMatcher
quoted_atom' =
    [ tok "['][']" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "[']" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|(x[a-fA-F0-9]+|[0-7]+)\\\\)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\'\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

root' :: TokenMatcher
root' =
    [ tokNext "^\\s*:-\\s" (Arbitrary "Punctuation") (GoTo directive')
    , tok "%.*?\\n" (Arbitrary "Comment")
    , tok "/\\*(.|\\n)*?\\*/" (Arbitrary "Comment")
    , tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "0'." (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0b[01]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0o[0-7]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0x[0-9a-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\d+\\.?\\d*((e|E)(\\+|-)?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "([A-Z_][a-zA-Z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(after|before)(?=[(])" (Arbitrary "Keyword")
    , tok "(parameter|this|se(lf|nder))(?=[(])" (Arbitrary "Keyword")
    , tok "(current_predicate|predicate_property)(?=[(])" (Arbitrary "Keyword")
    , tok "(expand_(goal|term)|(goal|term)_expansion|phrase)(?=[(])" (Arbitrary "Keyword")
    , tok "(abolish|c(reate|urrent))_(object|protocol|category)(?=[(])" (Arbitrary "Keyword")
    , tok "(object|protocol|category)_property(?=[(])" (Arbitrary "Keyword")
    , tok "complements_object(?=[(])" (Arbitrary "Keyword")
    , tok "extends_(object|protocol|category)(?=[(])" (Arbitrary "Keyword")
    , tok "imp(lements_protocol|orts_category)(?=[(])" (Arbitrary "Keyword")
    , tok "(instantiat|specializ)es_class(?=[(])" (Arbitrary "Keyword")
    , tok "(current_event|(abolish|define)_events)(?=[(])" (Arbitrary "Keyword")
    , tok "(current|set)_logtalk_flag(?=[(])" (Arbitrary "Keyword")
    , tok "logtalk_(compile|l(ibrary_path|oad))(?=[(])" (Arbitrary "Keyword")
    , tok "(clause|retract(all)?)(?=[(])" (Arbitrary "Keyword")
    , tok "a(bolish|ssert(a|z))(?=[(])" (Arbitrary "Keyword")
    , tok "(ca(ll|tch)|throw)(?=[(])" (Arbitrary "Keyword")
    , tok "(fail|true)\\b" (Arbitrary "Keyword")
    , tok "((bag|set)of|f(ind|or)all)(?=[(])" (Arbitrary "Keyword")
    , tok "threaded(_(call|once|ignore|exit|peek|wait|notify))?(?=[(])" (Arbitrary "Keyword")
    , tok "unify_with_occurs_check(?=[(])" (Arbitrary "Keyword")
    , tok "(functor|arg|copy_term)(?=[(])" (Arbitrary "Keyword")
    , tok "(rem|mod|abs|sign)(?=[(])" (Arbitrary "Keyword")
    , tok "float(_(integer|fractional)_part)?(?=[(])" (Arbitrary "Keyword")
    , tok "(floor|truncate|round|ceiling)(?=[(])" (Arbitrary "Keyword")
    , tok "(cos|atan|exp|log|s(in|qrt))(?=[(])" (Arbitrary "Keyword")
    , tok "(var|atom(ic)?|integer|float|compound|n(onvar|umber))(?=[(])" (Arbitrary "Keyword")
    , tok "(curren|se)t_(in|out)put(?=[(])" (Arbitrary "Keyword")
    , tok "(open|close)(?=[(])" (Arbitrary "Keyword")
    , tok "flush_output(?=[(])" (Arbitrary "Keyword")
    , tok "(at_end_of_stream|flush_output)\\b" (Arbitrary "Keyword")
    , tok "(stream_property|at_end_of_stream|set_stream_position)(?=[(])" (Arbitrary "Keyword")
    , tok "(nl|(get|peek|put)_(byte|c(har|ode)))(?=[(])" (Arbitrary "Keyword")
    , tok "\\bnl\\b" (Arbitrary "Keyword")
    , tok "read(_term)?(?=[(])" (Arbitrary "Keyword")
    , tok "write(q|_(canonical|term))?(?=[(])" (Arbitrary "Keyword")
    , tok "(current_)?op(?=[(])" (Arbitrary "Keyword")
    , tok "(current_)?char_conversion(?=[(])" (Arbitrary "Keyword")
    , tok "atom_(length|c(hars|o(ncat|des)))(?=[(])" (Arbitrary "Keyword")
    , tok "(char_code|sub_atom)(?=[(])" (Arbitrary "Keyword")
    , tok "number_c(har|ode)s(?=[(])" (Arbitrary "Keyword")
    , tok "(se|curren)t_prolog_flag(?=[(])" (Arbitrary "Keyword")
    , tok "\\bhalt\\b" (Arbitrary "Keyword")
    , tok "halt(?=[(])" (Arbitrary "Keyword")
    , tok "(::|:|\\^\\^)" (Arbitrary "Operator")
    , tok "[{}]" (Arbitrary "Keyword")
    , tok "\\bonce(?=[(])" (Arbitrary "Keyword")
    , tok "\\brepeat\\b" (Arbitrary "Keyword")
    , tok "(>>|<<|/\\\\|\\\\\\\\|\\\\)" (Arbitrary "Operator")
    , tok "\\bis\\b" (Arbitrary "Keyword")
    , tok "(=:=|=\\\\=|<|=<|>=|>)" (Arbitrary "Operator")
    , tok "=\\.\\." (Arbitrary "Operator")
    , tok "(=|\\\\=)" (Arbitrary "Operator")
    , tok "(==|\\\\==|@=<|@<|@>=|@>)" (Arbitrary "Operator")
    , tok "(//|[-+*/])" (Arbitrary "Operator")
    , tok "\\b(mod|rem)\\b" (Arbitrary "Operator")
    , tok "\\b\\*\\*\\b" (Arbitrary "Operator")
    , tok "-->" (Arbitrary "Operator")
    , tok "([!;]|->)" (Arbitrary "Operator")
    , tok "\\\\+" (Arbitrary "Operator")
    , tok "[?@]" (Arbitrary "Operator")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "[()\\[\\],.|]" (Arbitrary "Text")
    , tok "[a-z][a-zA-Z0-9_]*" (Arbitrary "Text")
    , tokNext "[']" (Arbitrary "Literal" :. Arbitrary "String") (GoTo quoted_atom')
    ]

directive' :: TokenMatcher
directive' =
    [ tokNext "(el)?if(?=[(])" (Arbitrary "Keyword") (GoTo root')
    , tokNext "(e(lse|ndif))[.]" (Arbitrary "Keyword") (GoTo root')
    , tokNext "(category|object|protocol)(?=[(])" (Arbitrary "Keyword") (GoTo entityrelations')
    , tokNext "(end_(category|object|protocol))[.]" (Arbitrary "Keyword") (GoTo root')
    , tokNext "(public|protected|private)(?=[(])" (Arbitrary "Keyword") (GoTo root')
    , tokNext "e(n(coding|sure_loaded)|xport)(?=[(])" (Arbitrary "Keyword") (GoTo root')
    , tokNext "in(fo|itialization)(?=[(])" (Arbitrary "Keyword") (GoTo root')
    , tokNext "(dynamic|synchronized|threaded)[.]" (Arbitrary "Keyword") (GoTo root')
    , tokNext "(alias|d(ynamic|iscontiguous)|m(eta_predicate|ode|ultifile)|s(et_(logtalk|prolog)_flag|ynchronized))(?=[(])" (Arbitrary "Keyword") (GoTo root')
    , tokNext "op(?=[(])" (Arbitrary "Keyword") (GoTo root')
    , tokNext "(calls|reexport|use(s|_module))(?=[(])" (Arbitrary "Keyword") (GoTo root')
    , tokNext "[a-z][a-zA-Z0-9_]*(?=[(])" (Arbitrary "Text") (GoTo root')
    , tokNext "[a-z][a-zA-Z0-9_]*[.]" (Arbitrary "Text") (GoTo root')
    ]

entityrelations' :: TokenMatcher
entityrelations' =
    [ tok "(extends|i(nstantiates|mp(lements|orts))|specializes)(?=[(])" (Arbitrary "Keyword")
    , tok "0'." (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0b[01]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0o[0-7]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "0x[0-9a-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\d+\\.?\\d*((e|E)(\\+|-)?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "([A-Z_][a-zA-Z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[a-z][a-zA-Z0-9_]*" (Arbitrary "Text")
    , tokNext "[']" (Arbitrary "Literal" :. Arbitrary "String") (GoTo quoted_atom')
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tokNext "([)]\\.)" (Arbitrary "Text") (GoTo root')
    , tok "(::)" (Arbitrary "Operator")
    , tok "[()\\[\\],.|]" (Arbitrary "Text")
    , tok "%.*?\\n" (Arbitrary "Comment")
    , tok "/\\*(.|\\n)*?\\*/" (Arbitrary "Comment")
    , tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    ]

