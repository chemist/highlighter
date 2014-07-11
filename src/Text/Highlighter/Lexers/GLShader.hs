module Text.Highlighter.Lexers.GLShader (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "GLSL"
    , lAliases = ["glsl"]
    , lExtensions = [".vert", ".frag", ".geo"]
    , lMimetypes = ["text/x-glslsrc"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "^#.*" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "//.*" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/(\\\\\\n)?[*](.|\\n)*?[*](\\\\\\n)?/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "\\+|-|\126|!=?|\\*|/|%|<<|>>|<=?|>=?|==?|&&?|\\^|\\|\\|?" (Arbitrary "Operator")
    , tok "[?:]" (Arbitrary "Operator")
    , tok "\\bdefined\\b" (Arbitrary "Operator")
    , tok "[;{}(),\\[\\]]" (Arbitrary "Punctuation")
    , tok "[+-]?\\d*\\.\\d+([eE][-+]?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "[+-]?\\d+\\.\\d*([eE][-+]?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[xX][0-9a-fA-F]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "0[0-7]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "[1-9][0-9]*" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\\b(attribute|const|uniform|varying|centroid|break|continue|do|for|while|if|else|in|out|inout|float|int|void|bool|true|false|invariant|discard|return|mat[234]|mat[234]x[234]|vec[234]|[ib]vec[234]|sampler[123]D|samplerCube|sampler[12]DShadow|struct)\\b" (Arbitrary "Keyword")
    , tok "\\b(asm|class|union|enum|typedef|template|this|packed|goto|switch|default|inline|noinline|volatile|public|static|extern|external|interface|long|short|double|half|fixed|unsigned|lowp|mediump|highp|precision|input|output|hvec[234]|[df]vec[234]|sampler[23]DRect|sampler2DRectShadow|sizeof|cast|namespace|using)\\b" (Arbitrary "Keyword")
    , tok "[a-zA-Z_][a-zA-Z_0-9]*" (Arbitrary "Name")
    , tok "\\." (Arbitrary "Punctuation")
    , tok "\\s+" (Arbitrary "Text")
    ]

