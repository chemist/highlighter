module Text.Highlighter.Lexers.Verilog (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "verilog"
    , lAliases = ["v"]
    , lExtensions = [".v", ".sv"]
    , lMimetypes = ["text/x-verilog"]
    , lStart = root'
    , lFlags = [multiline]
    }

classname' :: TokenMatcher
classname' =
    [ tokNext "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Class") Pop
    ]

macro' :: TokenMatcher
macro' =
    [ tok "[^/\\n]+" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "/[*](.|\\n)*?[*]/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single") Pop
    , tok "/" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "(?<=\\\\)\\n" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext "\\n" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    ]

root' :: TokenMatcher
root' =
    [ tokNext "^\\s*`define" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo macro')
    , tok "\\n" (Arbitrary "Text")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\\\\\n" (Arbitrary "Text")
    , tok "/(\\\\\\n)?/(\\n|(.|\\n)*?[^\\\\]\\n)" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "[{}#@]" (Arbitrary "Punctuation")
    , tokNext "L?\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "L?'(\\\\.|\\\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\\\\'\\n])'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+)[eE][+-]?\\d+[lL]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "(\\d+\\.\\d*|\\.\\d+|\\d+[fF])[fF]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "([0-9]+)|(\\'h)[0-9a-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "([0-9]+)|(\\'b)[0-1]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "([0-9]+)|(\\'d)[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "([0-9]+)|(\\'o)[0-7]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "\\'[01xz]" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\\d+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\*/" (Arbitrary "Error")
    , tok "[\126!%^&*+=|?:<>/-]" (Arbitrary "Operator")
    , tok "[()\\[\\],.;\\']" (Arbitrary "Punctuation")
    , tok "`[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "^\\s*(package)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")])
    , tokNext "^\\s*(import)(\\s+)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text")]) (GoTo import')
    , tok "(always|always_comb|always_ff|always_latch|and|assign|automatic|begin|break|buf|bufif0|bufif1|case|casex|casez|cmos|const|continue|deassign|default|defparam|disable|do|edge|else|end|endcase|endfunction|endgenerate|endmodule|endpackage|endprimitive|endspecify|endtable|endtask|enum|event|final|for|force|forever|fork|function|generate|genvar|highz0|highz1|if|initial|inout|input|integer|join|large|localparam|macromodule|medium|module|nand|negedge|nmos|nor|not|notif0|notif1|or|output|packed|parameter|pmos|posedge|primitive|pull0|pull1|pulldown|pullup|rcmos|ref|release|repeat|return|rnmos|rpmos|rtran|rtranif0|rtranif1|scalared|signed|small|specify|specparam|strength|string|strong0|strong1|struct|table|task|tran|tranif0|tranif1|type|typedef|unsigned|var|vectored|void|wait|weak0|weak1|while|xnor|xor)\\b" (Arbitrary "Keyword")
    , tok "(`accelerate|`autoexpand_vectornets|`celldefine|`default_nettype|`else|`elsif|`endcelldefine|`endif|`endprotect|`endprotected|`expand_vectornets|`ifdef|`ifndef|`include|`noaccelerate|`noexpand_vectornets|`noremove_gatenames|`noremove_netnames|`nounconnected_drive|`protect|`protected|`remove_gatenames|`remove_netnames|`resetall|`timescale|`unconnected_drive|`undef)\\b" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "(\\$bits|\\$bitstoreal|\\$bitstoshortreal|\\$countdrivers|\\$display|\\$fclose|\\$fdisplay|\\$finish|\\$floor|\\$fmonitor|\\$fopen|\\$fstrobe|\\$fwrite|\\$getpattern|\\$history|\\$incsave|\\$input|\\$itor|\\$key|\\$list|\\$log|\\$monitor|\\$monitoroff|\\$monitoron|\\$nokey|\\$nolog|\\$printtimescale|\\$random|\\$readmemb|\\$readmemh|\\$realtime|\\$realtobits|\\$reset|\\$reset_count|\\$reset_value|\\$restart|\\$rtoi|\\$save|\\$scale|\\$scope|\\$shortrealtobits|\\$showscopes|\\$showvariables|\\$showvars|\\$sreadmemb|\\$sreadmemh|\\$stime|\\$stop|\\$strobe|\\$time|\\$timeformat|\\$write)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tokNext "(class)(\\s+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text")]) (GoTo classname')
    , tok "(byte|shortint|int|longint|interger|time|bit|logic|reg|supply0|supply1|tri|triand|trior|tri0|tri1|trireg|uwire|wire|wand|worshortreal|real|realtime)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*:(?!:)" (Arbitrary "Name" :. Arbitrary "Label")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\\\\([\\\\abfnrtv\"\\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\\\\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\\\n" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\\\" (Arbitrary "Literal" :. Arbitrary "String")
    ]

import' :: TokenMatcher
import' =
    [ tokNext "[a-zA-Z0-9_:]+\\*?" (Arbitrary "Name" :. Arbitrary "Namespace") Pop
    ]

