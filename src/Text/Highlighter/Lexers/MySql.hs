module Text.Highlighter.Lexers.MySql (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "MySQL"
    , lAliases = ["mysql"]
    , lExtensions = []
    , lMimetypes = ["text/x-mysql"]
    , lStart = root'
    , lFlags = [caseless]
    }

multilineComments' :: TokenMatcher
multilineComments' =
    [ tokNext "/\\*" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo multilineComments')
    , tokNext "\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline") Pop
    , tok "[^/\\*]+" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "[/*]" (Arbitrary "Comment" :. Arbitrary "Multiline")
    ]

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "(#|--\\s+).*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tokNext "/\\*" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo multilineComments')
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[0-9]*\\.[0-9]+(e[+-][0-9]+)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "'(''|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "\"(\"\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "`(``|[^`])*`" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "[+*/<>=\126!@#%^&|`?^-]" (Arbitrary "Operator")
    , tok "\\b(tinyint|smallint|mediumint|int|integer|bigint|date|datetime|time|bit|bool|tinytext|mediumtext|longtext|text|tinyblob|mediumblob|longblob|blob|float|double|double\\s+precision|real|numeric|dec|decimal|timestamp|year|char|varchar|varbinary|varcharacter|enum|set)(\\b\\s*)(\\()?" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Type"), (Arbitrary "Text"), (Arbitrary "Punctuation")])
    , tok "\\b(add|all|alter|analyze|and|as|asc|asensitive|before|between|bigint|binary|blob|both|by|call|cascade|case|change|char|character|check|collate|column|condition|constraint|continue|convert|create|cross|current_date|current_time|current_timestamp|current_user|cursor|database|databases|day_hour|day_microsecond|day_minute|day_second|dec|decimal|declare|default|delayed|delete|desc|describe|deterministic|distinct|distinctrow|div|double|drop|dual|each|else|elseif|enclosed|escaped|exists|exit|explain|fetch|float|float4|float8|for|force|foreign|from|fulltext|grant|group|having|high_priority|hour_microsecond|hour_minute|hour_second|if|ignore|in|index|infile|inner|inout|insensitive|insert|int|int1|int2|int3|int4|int8|integer|interval|into|is|iterate|join|key|keys|kill|leading|leave|left|like|limit|lines|load|localtime|localtimestamp|lock|long|loop|low_priority|match|minute_microsecond|minute_second|mod|modifies|natural|no_write_to_binlog|not|numeric|on|optimize|option|optionally|or|order|out|outer|outfile|precision|primary|procedure|purge|raid0|read|reads|real|references|regexp|release|rename|repeat|replace|require|restrict|return|revoke|right|rlike|schema|schemas|second_microsecond|select|sensitive|separator|set|show|smallint|soname|spatial|specific|sql|sql_big_result|sql_calc_found_rows|sql_small_result|sqlexception|sqlstate|sqlwarning|ssl|starting|straight_join|table|terminated|then|to|trailing|trigger|undo|union|unique|unlock|unsigned|update|usage|use|using|utc_date|utc_time|utc_timestamp|values|varying|when|where|while|with|write|x509|xor|year_month|zerofill)\\b" (Arbitrary "Keyword")
    , tok "\\b(auto_increment|engine|charset|tables)\\b" (Arbitrary "Keyword" :. Arbitrary "Pseudo")
    , tok "(true|false|null)" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "([a-zA-Z_][a-zA-Z0-9_]*)(\\s*)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Punctuation")])
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    , tok "@[A-Za-z0-9]*[._]*[A-Za-z0-9]*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[;:()\\[\\],\\.]" (Arbitrary "Punctuation")
    ]

