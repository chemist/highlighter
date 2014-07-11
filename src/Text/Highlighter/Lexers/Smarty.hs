module Text.Highlighter.Lexers.Smarty (lexer) where
import qualified Text.Highlighter.Lexers.Php as Php
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Smarty"
    , lAliases = ["smarty"]
    , lExtensions = [".tpl"]
    , lMimetypes = ["application/x-smarty"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

root' :: TokenMatcher
root' =
    [ tok "[^{]+" (Arbitrary "Other")
    , tok "(\\{)(\\*.*?\\*)(\\})" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Comment"), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tok "(\\{php\\})(.*?)(\\{/php\\})" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Using Php.lexer), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tokNext "(\\{)(/?[a-zA-Z_][a-zA-Z0-9_]*)(\\s*)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text")]) (GoTo smarty')
    , tokNext "\\{" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo smarty')
    ]

smarty' :: TokenMatcher
smarty' =
    [ tok "\\s+" (Arbitrary "Text")
    , tokNext "\\}" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok "#[a-zA-Z_][a-zA-Z0-9_]*#" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "\\$[a-zA-Z_][a-zA-Z0-9_]*(\\.[a-zA-Z0-9_]+)*" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "[\126!%^&*()+=|\\[\\]:;,.<>/?{}@-]" (Arbitrary "Operator")
    , tok "(true|false|null)\8" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "[0-9](\\.[0-9]*)?(eE[+-][0-9])?[flFLdD]?|0[xX][0-9a-fA-F]+[Ll]?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Attribute")
    ]

