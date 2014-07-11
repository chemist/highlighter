module Text.Highlighter.Lexers.Vim (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "VimL"
    , lAliases = ["vim"]
    , lExtensions = [".vim", ".vimrc"]
    , lMimetypes = ["text/x-vim"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "^\\s*\".*" (Arbitrary "Comment")
    , tok "(?<=\\s)\"[^\\-:.%#=*].*" (Arbitrary "Comment")
    , tok "[ \\t]+" (Arbitrary "Text")
    , tok "/(\\\\\\\\|\\\\/|[^\\n/])*/" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "\"(\\\\\\\\|\\\\\"|[^\\n\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(\\\\\\\\|\\\\'|[^\\n'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "-?\\d+" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "#[0-9a-f]{6}" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "^:" (Arbitrary "Punctuation")
    , tok "[()<>+=!|,\126-]" (Arbitrary "Punctuation")
    , tok "\\b(let|if|else|endif|elseif|fun|function|endfunction)\\b" (Arbitrary "Keyword")
    , tok "\\b(NONE|bold|italic|underline|dark|light)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "\\b\\w+\\b" (Arbitrary "Name" :. Arbitrary "Other")
    , tok "." (Arbitrary "Text")
    ]

