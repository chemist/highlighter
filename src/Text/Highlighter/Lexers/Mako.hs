module Text.Highlighter.Lexers.Mako (lexer) where
import qualified Text.Highlighter.Lexers.Python as Python
import qualified Text.Highlighter.Lexers.Python as Python
import qualified Text.Highlighter.Lexers.Python as Python
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Mako"
    , lAliases = ["mako"]
    , lExtensions = [".mao"]
    , lMimetypes = ["application/x-mako"]
    , lStart = root'
    , lFlags = [multiline]
    }

ondeftags' :: TokenMatcher
ondeftags' =
    [ tok "<%" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tok "(?<=<%)(include|inherit|namespace|page)" (Arbitrary "Name" :. Arbitrary "Builtin")
    , anyOf tag'
    ]

tag' :: TokenMatcher
tag' =
    [ tok "((?:\\w+)\\s*=)\\s*(\".*?\")" (ByGroups [(Arbitrary "Name" :. Arbitrary "Attribute"), (Arbitrary "Literal" :. Arbitrary "String")])
    , tokNext "/?\\s*>" (Arbitrary "Comment" :. Arbitrary "Preproc") Pop
    , tok "\\s+" (Arbitrary "Text")
    ]

root' :: TokenMatcher
root' =
    [ tok "(\\s*)(%)(\\s*end(?:\\w+))(\\n|\\Z)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Keyword"), (Arbitrary "Other")])
    , tok "(\\s*)(%)([^\\n]*)(\\n|\\Z)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc"), (Using Python.lexer), (Arbitrary "Other")])
    , tok "(\\s*)(##[^\\n]*)(\\n|\\Z)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Other")])
    , tok "(?s)<%doc>.*?</%doc>" (Arbitrary "Comment" :. Arbitrary "Preproc")
    , tokNext "(<%)([\\w\\.\\:]+)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Name" :. Arbitrary "Builtin")]) (GoTo tag')
    , tok "(</%)([\\w\\.\\:]+)(>)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Name" :. Arbitrary "Builtin"), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tokNext "<%(?=([\\w\\.\\:]+))" (Arbitrary "Comment" :. Arbitrary "Preproc") (GoTo ondeftags')
    , tok "(<%(?:!?))(.*?)(%>)(?s)" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Using Python.lexer), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tok "(\\$\\{)(.*?)(\\})" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Using Python.lexer), (Arbitrary "Comment" :. Arbitrary "Preproc")])
    , tok "(?sx)\10                (.+?)                # anything, followed by:\10                (?:\10                 (?<=\\n)(?=%|\\#\\#) | # an eval or comment line\10                 (?=\\#\\*) |          # multiline comment\10                 (?=</?%) |          # a python block\10                                     # call start or end\10                 (?=\\$\\{) |          # a substitution\10                 (?<=\\n)(?=\\s*%) |\10                                     # - don't consume\10                 (\\\\\\n) |            # an escaped newline\10                 \\Z                  # end of string\10                )\10            " (ByGroups [(Arbitrary "Other"), (Arbitrary "Operator")])
    , tok "\\s+" (Arbitrary "Text")
    ]

attr' :: TokenMatcher
attr' =
    [ tokNext "\".*?\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tokNext "'.*?'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tokNext "[^\\s>]+" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

