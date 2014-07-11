module Text.Highlighter.Lexers.Llvm (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "LLVM"
    , lAliases = ["llvm"]
    , lExtensions = [".ll"]
    , lMimetypes = ["text/x-llvm"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tok "^\\s*([-a-zA-Z$._][-a-zA-Z$._0-9]*|\"[^\"]*?\")\\s*:" (Arbitrary "Name" :. Arbitrary "Label")
    , anyOf keyword'
    , tok "%([-a-zA-Z$._][-a-zA-Z$._0-9]*|\"[^\"]*?\")" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "@([-a-zA-Z$._][-a-zA-Z$._0-9]*|\"[^\"]*?\")" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Global")
    , tok "%\\d+" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Anonymous")
    , tok "@\\d+" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Global")
    , tok "!([-a-zA-Z$._][-a-zA-Z$._0-9]*|\"[^\"]*?\")" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "!\\d+" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Anonymous")
    , tok "c?\"[^\"]*?\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "0[xX][a-fA-F0-9]+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "-?\\d+(?:[.]\\d+)?(?:[eE][-+]?\\d+(?:[.]\\d+)?)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[=<>{}\\[\\]()*.,!]|x\\b" (Arbitrary "Punctuation")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "(\\n|\\s)+" (Arbitrary "Text")
    , tok ";.*?\\n" (Arbitrary "Comment")
    ]

keyword' :: TokenMatcher
keyword' =
    [ tok "(begin|end|true|false|declare|define|global|constant|private|linker_private|internal|available_externally|linkonce|linkonce_odr|weak|weak_odr|appending|dllimport|dllexport|common|default|hidden|protected|extern_weak|external|thread_local|zeroinitializer|undef|null|to|tail|target|triple|deplibs|datalayout|volatile|nuw|nsw|exact|inbounds|align|addrspace|section|alias|module|asm|sideeffect|gc|dbg|ccc|fastcc|coldcc|x86_stdcallcc|x86_fastcallcc|arm_apcscc|arm_aapcscc|arm_aapcs_vfpcc|cc|c|signext|zeroext|inreg|sret|nounwind|noreturn|noalias|nocapture|byval|nest|readnone|readonly|inlinehint|noinline|alwaysinline|optsize|ssp|sspreq|noredzone|noimplicitfloat|naked|type|opaque|eq|ne|slt|sgt|sle|sge|ult|ugt|ule|uge|oeq|one|olt|ogt|ole|oge|ord|uno|ueq|une|x|add|fadd|sub|fsub|mul|fmul|udiv|sdiv|fdiv|urem|srem|frem|shl|lshr|ashr|and|or|xor|icmp|fcmp|phi|call|trunc|zext|sext|fptrunc|fpext|uitofp|sitofp|fptouifptosi|inttoptr|ptrtoint|bitcast|select|va_arg|ret|br|switch|invoke|unwind|unreachable|malloc|alloca|free|load|store|getelementptr|extractelement|insertelement|shufflevector|getresult|extractvalue|insertvalue)\\b" (Arbitrary "Keyword")
    , tok "void|float|double|x86_fp80|fp128|ppc_fp128|label|metadata" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "i[1-9]\\d*" (Arbitrary "Keyword")
    ]

