module Text.Highlighter.Lexers.Hybris (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Hybris"
    , lAliases = ["hybris", "hy"]
    , lExtensions = [".hy", ".hyb"]
    , lMimetypes = ["text/x-hybris", "application/x-hybris"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

import' :: TokenMatcher
import' =
    [ tokNext "[a-zA-Z0-9_.]+\\*?" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "^(\\s*(?:function|method|operator\\s+)+?)([a-zA-Z_][a-zA-Z0-9_]*)(\\s*)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Operator")])
    , tok "[^\\S\\n]+" (Arbitrary "Text")
    , tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "@[a-zA-Z_][a-zA-Z0-9_\\.]*" (Arbitrary "Name" :. Arbitrary "Decorator")
    , tok "(break|case|catch|next|default|do|else|finally|for|foreach|of|unless|if|new|return|switch|me|throw|try|while)\\b" (Arbitrary "Keyword")
    , tok "(extends|private|protected|public|static|throws|function|method|operator)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "(true|false|null|__FILE__|__LINE__|__VERSION__|__LIB_PATH__|__INC_PATH__)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tokNext "(class|struct)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text")]) (GoTo class')
    , tokNext "(import|include)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")]) (GoTo import')
    , tok "(gc_collect|gc_mm_items|gc_mm_usage|gc_collect_threshold|urlencode|urldecode|base64encode|base64decode|sha1|crc32|sha2|md5|md5_file|acos|asin|atan|atan2|ceil|cos|cosh|exp|fabs|floor|fmod|log|log10|pow|sin|sinh|sqrt|tan|tanh|isint|isfloat|ischar|isstring|isarray|ismap|isalias|typeof|sizeof|toint|tostring|fromxml|toxml|binary|pack|load|eval|var_names|var_values|user_functions|dyn_functions|methods|call|call_method|mknod|mkfifo|mount|umount2|umount|ticks|usleep|sleep|time|strtime|strdate|dllopen|dlllink|dllcall|dllcall_argv|dllclose|env|exec|fork|getpid|wait|popen|pclose|exit|kill|pthread_create|pthread_create_argv|pthread_exit|pthread_join|pthread_kill|smtp_send|http_get|http_post|http_download|socket|bind|listen|accept|getsockname|getpeername|settimeout|connect|server|recv|send|close|print|println|printf|input|readline|serial_open|serial_fcntl|serial_get_attr|serial_get_ispeed|serial_get_ospeed|serial_set_attr|serial_set_ispeed|serial_set_ospeed|serial_write|serial_read|serial_close|xml_load|xml_parse|fopen|fseek|ftell|fsize|fread|fwrite|fgets|fclose|file|readdir|pcre_replace|size|pop|unmap|has|keys|values|length|find|substr|replace|split|trim|remove|contains|join)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(MethodReference|Runner|Dll|Thread|Pipe|Process|Runnable|CGI|ClientSocket|Socket|ServerSocket|File|Console|Directory|Exception)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "'\\\\.'|'[^\\\\]'|'\\\\u[0-9a-f]{4}'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(\\.)([a-zA-Z_][a-zA-Z0-9_]*)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Name" :. Arbitrary "Attribute")])
    , tok "[a-zA-Z_][a-zA-Z0-9_]*:" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "[a-zA-Z_\\$][a-zA-Z0-9_]*" (Arbitrary "Name")
    , tok "[\126\\^\\*!%&\\[\\]\\(\\)\\{\\}<>\\|+=:;,./?\\-@]+" (Arbitrary "Operator")
    , tok "[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-f]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9]+L?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\n" (Arbitrary "Text")
    ]

class' :: TokenMatcher
class' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

