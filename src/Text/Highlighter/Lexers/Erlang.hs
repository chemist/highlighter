module Text.Highlighter.Lexers.Erlang (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Erlang"
    , lAliases = ["erlang"]
    , lExtensions = [".erl", ".hrl"]
    , lMimetypes = ["text/x-erlang"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "%.*\\n" (Arbitrary "Comment")
    , tok "(after|begin|case|catch|cond|end|fun|if|let|of|query|receive|try|when)\\b" (Arbitrary "Keyword")
    , tok "(abs|append_element|apply|atom_to_list|binary_to_list|bitstring_to_list|binary_to_term|bit_size|bump_reductions|byte_size|cancel_timer|check_process_code|delete_module|demonitor|disconnect_node|display|element|erase|exit|float|float_to_list|fun_info|fun_to_list|function_exported|garbage_collect|get|get_keys|group_leader|hash|hd|integer_to_list|iolist_to_binary|iolist_size|is_atom|is_binary|is_bitstring|is_boolean|is_builtin|is_float|is_function|is_integer|is_list|is_number|is_pid|is_port|is_process_alive|is_record|is_reference|is_tuple|length|link|list_to_atom|list_to_binary|list_to_bitstring|list_to_existing_atom|list_to_float|list_to_integer|list_to_pid|list_to_tuple|load_module|localtime_to_universaltime|make_tuple|md5|md5_final|md5_update|memory|module_loaded|monitor|monitor_node|node|nodes|open_port|phash|phash2|pid_to_list|port_close|port_command|port_connect|port_control|port_call|port_info|port_to_list|process_display|process_flag|process_info|purge_module|put|read_timer|ref_to_list|register|resume_process|round|send|send_after|send_nosuspend|set_cookie|setelement|size|spawn|spawn_link|spawn_monitor|spawn_opt|split_binary|start_timer|statistics|suspend_process|system_flag|system_info|system_monitor|system_profile|term_to_binary|tl|trace|trace_delivered|trace_info|trace_pattern|trunc|tuple_size|tuple_to_list|universaltime_to_localtime|unlink|unregister|whereis)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(and|andalso|band|bnot|bor|bsl|bsr|bxor|div|not|or|orelse|rem|xor)\\b" (Arbitrary "Operator" :. Arbitrary "Word")
    , tokNext "^-" (Arbitrary "Punctuation") (GoTo directive')
    , tok "(\\+|-|\\*|/|<|>|=|==|/=|=:=|=/=|=<|>=|\\+\\+|--|<-|!)" (Arbitrary "Operator")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "<<" (Arbitrary "Name" :. Arbitrary "Label")
    , tok ">>" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "((?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]'))(:)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Namespace"), (Arbitrary "Punctuation")])
    , tok "^((?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]'))(\\s*)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Punctuation")])
    , tok "[+-]?(?:[2-9]|[12][0-9]|3[0-6])#[0-9a-zA-Z]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[+-]?\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[+-]?\\d+.\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[]\\[:_@\\\".{}()|;,]" (Arbitrary "Punctuation")
    , tok "(?:[A-Z_][a-zA-Z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]')" (Arbitrary "Name")
    , tok "\\?(?:(?:[A-Z_][a-zA-Z0-9_]*)|(?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]'))" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "\\$(?:(?:\\\\(?:[bdefnrstv\\'\"\\\\/]|[0-7][0-7]?[0-7]?|\\^[a-zA-Z]))|\\\\[ %]|[^\\\\])" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "#(?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]')(:?\\.(?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]'))?" (Arbitrary "Name" :. Arbitrary "Label")
    ]

string' :: TokenMatcher
string' =
    [ tok "(?:\\\\(?:[bdefnrstv\\'\"\\\\/]|[0-7][0-7]?[0-7]?|\\^[a-zA-Z]))" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\126[0-9.*]*[\126#+bBcdefginpPswWxX]" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol")
    , tok "[^\"\\\\\126]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\126" (Arbitrary "Literal" :. Arbitrary "String")
    ]

directive' :: TokenMatcher
directive' =
    [ tokNext "(define)(\\s*)(\\()((?:(?:[A-Z_][a-zA-Z0-9_]*)|(?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]')))" (ByGroups [(Arbitrary "Name" :. Arbitrary "Entity"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Constant")]) Pop
    , tokNext "(record)(\\s*)(\\()((?:(?:[A-Z_][a-zA-Z0-9_]*)|(?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]')))" (ByGroups [(Arbitrary "Name" :. Arbitrary "Entity"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Label")]) Pop
    , tokNext "(?:[a-z][a-zA-Z0-9_]*|'[^\\n']*[^\\\\]')" (Arbitrary "Name" :. Arbitrary "Entity") Pop
    ]

