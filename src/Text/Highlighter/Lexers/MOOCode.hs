module Text.Highlighter.Lexers.MOOCode (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "MOOCode"
    , lAliases = ["moocode"]
    , lExtensions = [".moo"]
    , lMimetypes = ["text/x-moocode"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "(0|[1-9][0-9_]*)" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "(E_PERM|E_DIV)" (Arbitrary "Name" :. Arbitrary "Exception")
    , tok "((#[-0-9]+)|(\\$[a-z_A-Z0-9]+))" (Arbitrary "Name" :. Arbitrary "Entity")
    , tok "\\b(if|else|elseif|endif|for|endfor|fork|endfork|while|endwhile|break|continue|return|try|except|endtry|finally|in)\\b" (Arbitrary "Keyword")
    , tok "(random|length)" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(player|caller|this|args)" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Instance")
    , tok "\\s+" (Arbitrary "Text")
    , tok "\\n" (Arbitrary "Text")
    , tok "([!;=,{}&\\|:\\.\\[\\]@\\(\\)\\<\\>\\?]+)" (Arbitrary "Operator")
    , tok "([a-z_A-Z0-9]+)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Operator")])
    , tok "([a-zA-Z_0-9]+)" (Arbitrary "Text")
    ]

