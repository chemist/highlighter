module Text.Highlighter.Lexers.Objdump (lexer) where
import qualified Text.Highlighter.Lexers.Gas as Gas
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "objdump"
    , lAliases = ["objdump"]
    , lExtensions = [".objdump"]
    , lMimetypes = ["text/x-objdump"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok "(.*?)(:)( +file format )(.*?)$" (ByGroups [(Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")])
    , tok "(Disassembly of section )(.*?)(:)$" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Punctuation")])
    , tok "([0-9A-Za-z]+)( )(<)(.*?)([-+])(0[xX][A-Za-z0-9]+)(>:)$" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Punctuation"), (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex"), (Arbitrary "Punctuation")])
    , tok "([0-9A-Za-z]+)( )(<)(.*?)(>:)$" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Punctuation")])
    , tok "( *)([0-9A-Za-z]+:)(\\t)((?:[0-9A-Za-z][0-9A-Za-z] )+)( *\9)([a-zA-Z].*?)$" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex"), (Arbitrary "Text"), (Using Gas.lexer)])
    , tok "( *)([0-9A-Za-z]+:)(\\t)((?:[0-9A-Za-z][0-9A-Za-z] )+)( *)(.*?)$" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "String")])
    , tok "( *)([0-9A-Za-z]+:)(\\t)((?:[0-9A-Za-z][0-9A-Za-z] )+)$" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text"), (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")])
    , tok "\9\\.\\.\\.$" (Arbitrary "Text")
    , tok "(\9\9\9)([0-9A-Za-z]+:)( )([^\9]+)(\9)(.*?)([-+])(0x[0-9A-Za-z]+)$" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Property"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Constant"), (Arbitrary "Punctuation"), (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")])
    , tok "(\9\9\9)([0-9A-Za-z]+:)( )([^\9]+)(\9)(.*?)$" (ByGroups [(Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Label"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Property"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Constant")])
    , tok "[^\10]+\10" (Arbitrary "Other")
    ]

