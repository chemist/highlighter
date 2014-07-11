module Text.Highlighter.Lexers.Duel (lexer) where
import qualified Text.Highlighter.Lexers.Javascript as Javascript
import qualified Text.Highlighter.Lexers.Html as Html
import qualified Text.Highlighter.Lexers.Javascript as Javascript
import qualified Text.Highlighter.Lexers.Html as Html
import qualified Text.Highlighter.Lexers.Html as Html
import qualified Text.Highlighter.Lexers.Html as Html
import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Duel"
    , lAliases = ["duel", "Duel Engine", "Duel View", "JBST", "jbst", "JsonML+BST"]
    , lExtensions = [".duel", ".jbst"]
    , lMimetypes = ["text/x-duel", "text/x-jbst"]
    , lStart = root'
    , lFlags = [dotall]
    }

root' :: TokenMatcher
root' =
    [ tok "(<%[@=#!:]?)(.*?)(%>)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Tag"), (Using Javascript.lexer), (Arbitrary "Name" :. Arbitrary "Tag")])
    , tok "(<%\\$)(.*?)(:)(.*?)(%>)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Tag"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Punctuation"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Name" :. Arbitrary "Tag")])
    , tok "(<%--)(.*?)(--%>)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Tag"), (Arbitrary "Comment" :. Arbitrary "Multiline"), (Arbitrary "Name" :. Arbitrary "Tag")])
    , tok "(<script.*?>)(.*?)(</script>)" (ByGroups [(Using Html.lexer), (Using Javascript.lexer), (Using Html.lexer)])
    , tok "(.+?)(?=<)" (Using Html.lexer)
    , tok ".+" (Using Html.lexer)
    ]

