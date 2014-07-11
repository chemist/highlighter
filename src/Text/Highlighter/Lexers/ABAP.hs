module Text.Highlighter.Lexers.ABAP (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "ABAP"
    , lAliases = ["abap"]
    , lExtensions = [".abap"]
    , lMimetypes = ["text/x-abap"]
    , lStart = root'
    , lFlags = [caseless, multiline]
    }

variableNames' :: TokenMatcher
variableNames' =
    [ tok "<[\\S_]+>" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[\\w][\\w_\126]*(?:(\\[\\])|->\\*)?" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

root' :: TokenMatcher
root' =
    [ anyOf common'
    , tok "(CALL\\s+(?:BADI|CUSTOMER-FUNCTION|FUNCTION))(\\s+)(\\'?\\S+\\'?)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")])
    , tok "(CALL\\s+(?:DIALOG|SCREEN|SUBSCREEN|SELECTION-SCREEN|TRANSACTION|TRANSFORMATION))\\b" (Arbitrary "Keyword")
    , tok "(FORM|PERFORM)(\\s+)([\\w_]+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")])
    , tok "(PERFORM)(\\s+)(\\()([\\w_]+)(\\))" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Punctuation")])
    , tok "(MODULE)(\\s+)(\\S+)(\\s+)(INPUT|OUTPUT)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Keyword")])
    , tok "(METHOD)(\\s+)([\\w_\126]+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")])
    , tok "(\\s+)([\\w_\\-]+)([=\\-]>)([\\w_\\-\126]+)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Operator"), (Arbitrary "Name" :. Arbitrary "Function")])
    , tok "(?<=(=|-)>)([\\w_\\-\126]+)(?=\\()" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "(ADD-CORRESPONDING|AUTHORITY-CHECK|CLASS-DATA|CLASS-EVENTS|CLASS-METHODS|CLASS-POOL|DELETE-ADJACENT|DIVIDE-CORRESPONDING|EDITOR-CALL|ENHANCEMENT-POINT|ENHANCEMENT-SECTION|EXIT-COMMAND|FIELD-GROUPS|FIELD-SYMBOLS|FUNCTION-POOL|INTERFACE-POOL|INVERTED-DATE|LOAD-OF-PROGRAM|LOG-POINT|MESSAGE-ID|MOVE-CORRESPONDING|MULTIPLY-CORRESPONDING|NEW-LINE|NEW-PAGE|NEW-SECTION|NO-EXTENSION|OUTPUT-LENGTH|PRINT-CONTROL|SELECT-OPTIONS|START-OF-SELECTION|SUBTRACT-CORRESPONDING|SYNTAX-CHECK|SYSTEM-EXCEPTIONS|TYPE-POOL|TYPE-POOLS)\\b" (Arbitrary "Keyword")
    , tok "CREATE\\s+(PUBLIC|PRIVATE|DATA|OBJECT)|((PUBLIC|PRIVATE|PROTECTED)\\s+SECTION|(TYPE|LIKE)(\\s+(LINE\\s+OF|REF\\s+TO|(SORTED|STANDARD|HASHED)\\s+TABLE\\s+OF))?|FROM\\s+(DATABASE|MEMORY)|CALL\\s+METHOD|(GROUP|ORDER) BY|HAVING|SEPARATED BY|GET\\s+(BADI|BIT|CURSOR|DATASET|LOCALE|PARAMETER|PF-STATUS|(PROPERTY|REFERENCE)\\s+OF|RUN\\s+TIME|TIME\\s+(STAMP)?)?|SET\\s+(BIT|BLANK\\s+LINES|COUNTRY|CURSOR|DATASET|EXTENDED\\s+CHECK|HANDLER|HOLD\\s+DATA|LANGUAGE|LEFT\\s+SCROLL-BOUNDARY|LOCALE|MARGIN|PARAMETER|PF-STATUS|PROPERTY\\s+OF|RUN\\s+TIME\\s+(ANALYZER|CLOCK\\s+RESOLUTION)|SCREEN|TITLEBAR|UPADTE\\s+TASK\\s+LOCAL|USER-COMMAND)|CONVERT\\s+((INVERTED-)?DATE|TIME|TIME\\s+STAMP|TEXT)|(CLOSE|OPEN)\\s+(DATASET|CURSOR)|(TO|FROM)\\s+(DATA BUFFER|INTERNAL TABLE|MEMORY ID|DATABASE|SHARED\\s+(MEMORY|BUFFER))|DESCRIBE\\s+(DISTANCE\\s+BETWEEN|FIELD|LIST|TABLE)|FREE\\s(MEMORY|OBJECT)?|PROCESS\\s+(BEFORE\\s+OUTPUT|AFTER\\s+INPUT|ON\\s+(VALUE-REQUEST|HELP-REQUEST))|AT\\s+(LINE-SELECTION|USER-COMMAND|END\\s+OF|NEW)|AT\\s+SELECTION-SCREEN(\\s+(ON(\\s+(BLOCK|(HELP|VALUE)-REQUEST\\s+FOR|END\\s+OF|RADIOBUTTON\\s+GROUP))?|OUTPUT))?|SELECTION-SCREEN:?\\s+((BEGIN|END)\\s+OF\\s+((TABBED\\s+)?BLOCK|LINE|SCREEN)|COMMENT|FUNCTION\\s+KEY|INCLUDE\\s+BLOCKS|POSITION|PUSHBUTTON|SKIP|ULINE)|LEAVE\\s+(LIST-PROCESSING|PROGRAM|SCREEN|TO LIST-PROCESSING|TO TRANSACTION)(ENDING|STARTING)\\s+AT|FORMAT\\s+(COLOR|INTENSIFIED|INVERSE|HOTSPOT|INPUT|FRAMES|RESET)|AS\\s+(CHECKBOX|SUBSCREEN|WINDOW)|WITH\\s+(((NON-)?UNIQUE)?\\s+KEY|FRAME)|(BEGIN|END)\\s+OF|DELETE(\\s+ADJACENT\\s+DUPLICATES\\sFROM)?|COMPARING(\\s+ALL\\s+FIELDS)?|INSERT(\\s+INITIAL\\s+LINE\\s+INTO|\\s+LINES\\s+OF)?|IN\\s+((BYTE|CHARACTER)\\s+MODE|PROGRAM)|END-OF-(DEFINITION|PAGE|SELECTION)|WITH\\s+FRAME(\\s+TITLE)|AND\\s+(MARK|RETURN)|CLIENT\\s+SPECIFIED|CORRESPONDING\\s+FIELDS\\s+OF|IF\\s+FOUND|FOR\\s+EVENT|INHERITING\\s+FROM|LEAVE\\s+TO\\s+SCREEN|LOOP\\s+AT\\s+(SCREEN)?|LOWER\\s+CASE|MATCHCODE\\s+OBJECT|MODIF\\s+ID|MODIFY\\s+SCREEN|NESTING\\s+LEVEL|NO\\s+INTERVALS|OF\\s+STRUCTURE|RADIOBUTTON\\s+GROUP|RANGE\\s+OF|REF\\s+TO|SUPPRESS DIALOG|TABLE\\s+OF|UPPER\\s+CASE|TRANSPORTING\\s+NO\\s+FIELDS|VALUE\\s+CHECK|VISIBLE\\s+LENGTH|HEADER\\s+LINE)\\b" (Arbitrary "Keyword")
    , tok "(^|(?<=(\\s|\\.)))(ABBREVIATED|ADD|ALIASES|APPEND|ASSERT|ASSIGN(ING)?|AT(\\s+FIRST)?|BACK|BLOCK|BREAK-POINT|CASE|CATCH|CHANGING|CHECK|CLASS|CLEAR|COLLECT|COLOR|COMMIT|CREATE|COMMUNICATION|COMPONENTS?|COMPUTE|CONCATENATE|CONDENSE|CONSTANTS|CONTEXTS|CONTINUE|CONTROLS|DATA|DECIMALS|DEFAULT|DEFINE|DEFINITION|DEFERRED|DEMAND|DETAIL|DIRECTORY|DIVIDE|DO|ELSE(IF)?|ENDAT|ENDCASE|ENDCLASS|ENDDO|ENDFORM|ENDFUNCTION|ENDIF|ENDLOOP|ENDMETHOD|ENDMODULE|ENDSELECT|ENDTRY|ENHANCEMENT|EVENTS|EXCEPTIONS|EXIT|EXPORT|EXPORTING|EXTRACT|FETCH|FIELDS?|FIND|FOR|FORM|FORMAT|FREE|FROM|HIDE|ID|IF|IMPORT|IMPLEMENTATION|IMPORTING|IN|INCLUDE|INCLUDING|INDEX|INFOTYPES|INITIALIZATION|INTERFACE|INTERFACES|INTO|LENGTH|LINES|LOAD|LOCAL|JOIN|KEY|MAXIMUM|MESSAGE|METHOD[S]?|MINIMUM|MODULE|MODIFY|MOVE|MULTIPLY|NODES|OBLIGATORY|OF|OFF|ON|OVERLAY|PACK|PARAMETERS|PERCENTAGE|POSITION|PROGRAM|PROVIDE|PUBLIC|PUT|RAISE|RAISING|RANGES|READ|RECEIVE|REFRESH|REJECT|REPORT|RESERVE|RESUME|RETRY|RETURN|RETURNING|RIGHT|ROLLBACK|SCROLL|SEARCH|SELECT|SHIFT|SINGLE|SKIP|SORT|SPLIT|STATICS|STOP|SUBMIT|SUBTRACT|SUM|SUMMARY|SUMMING|SUPPLY|TABLE|TABLES|TIMES|TITLE|TO|TOP-OF-PAGE|TRANSFER|TRANSLATE|TRY|TYPES|ULINE|UNDER|UNPACK|UPDATE|USING|VALUE|VALUES|VIA|WAIT|WHEN|WHERE|WHILE|WITH|WINDOW|WRITE)\\b" (Arbitrary "Keyword")
    , tok "(abs|acos|asin|atan|boolc|boolx|bit_set|char_off|charlen|ceil|cmax|cmin|condense|contains|contains_any_of|contains_any_not_of|concat_lines_of|cos|cosh|count|count_any_of|count_any_not_of|dbmaxlen|distance|escape|exp|find|find_end|find_any_of|find_any_not_of|floor|frac|from_mixed|insert|lines|log|log10|match|matches|nmax|nmin|numofchar|repeat|replace|rescale|reverse|round|segment|shift_left|shift_right|sign|sin|sinh|sqrt|strlen|substring|substring_after|substring_from|substring_before|substring_to|tan|tanh|to_upper|to_lower|to_mixed|translate|trunc|xstrlen)(\\()\\b" (ByGroups [(Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Punctuation")])
    , tok "&[0-9]" (Arbitrary "Name")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "(?<=(\\s|.))(AND|EQ|NE|GT|LT|GE|LE|CO|CN|CA|NA|CS|NOT|NS|CP|NP|BYTE-CO|BYTE-CN|BYTE-CA|BYTE-NA|BYTE-CS|BYTE-NS|IS\\s+(NOT\\s+)?(INITIAL|ASSIGNED|REQUESTED|BOUND))\\b" (Arbitrary "Operator")
    , anyOf variableNames'
    , tok "[?*<>=\\-+]" (Arbitrary "Operator")
    , tok "'(''|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "[/;:()\\[\\],\\.]" (Arbitrary "Punctuation")
    ]

common' :: TokenMatcher
common' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "^\\*.*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "\\\".*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    ]

