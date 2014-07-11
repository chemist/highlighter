module Text.Highlighter.Lexers.Haxe (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "haXe"
    , lAliases = ["hx", "haXe"]
    , lExtensions = [".hx"]
    , lMimetypes = ["text/haxe"]
    , lStart = root'
    , lFlags = [multiline, dotall]
    }

anonfundef' :: TokenMatcher
anonfundef' =
    [ tokNext "\\bfunction\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration") (GoTo fundecl')
    ]

instancefundef' :: TokenMatcher
instancefundef' =
    [ tok "(?:public|private|override|static|inline|extern|dynamic)" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tokNext "\\b(function)(\\s+)((?:[a-zA-Z_][a-zA-Z0-9_]*))" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Function")]) (GoTo fundecl')
    ]

typedecl' :: TokenMatcher
typedecl' =
    [ anyOf whitespace'
    , tok "(?:(?:[a-z0-9_\\.])*[A-Z_][A-Za-z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Class")
    , tokNext "<" (Arbitrary "Punctuation") (GoTo generictypedecl')
    , tokNext "(?=[{}()=,a-z])" (Arbitrary "Text") Pop
    ]

typedefbody' :: TokenMatcher
typedefbody' =
    [ anyOf whitespace'
    , anyOf instancevardef'
    , anyOf instancefundef'
    , tokNext ">" (Arbitrary "Punctuation") (GoTo typedecl')
    , tok "," (Arbitrary "Punctuation")
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

classdefbody' :: TokenMatcher
classdefbody' =
    [ anyOf whitespace'
    , anyOf instancevardef'
    , anyOf instancefundef'
    , tokNext "}" (Arbitrary "Punctuation") Pop
    , anyOf codeblock'
    ]

fundecl' :: TokenMatcher
fundecl' =
    [ anyOf whitespace'
    , anyOf typelabel'
    , anyOf generictypedecl'
    , tokNext "\\(" (Arbitrary "Punctuation") (GoTo funargdecl')
    , tokNext "(?=[a-zA-Z0-9_])" (Arbitrary "Text") Pop
    , tokNext "{" (Arbitrary "Punctuation") (DoAll [Pop, (GoTo codeblock')])
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

interfacedef' :: TokenMatcher
interfacedef' =
    [ tokNext "interface" (Arbitrary "Keyword" :. Arbitrary "Declaration") (DoAll [(GoTo interfacedefprebody'), (GoTo typedecl')])
    ]

codekeywords' :: TokenMatcher
codekeywords' =
    [ tok "\\b(if|else|while|do|for|in|break|continue|return|switch|case|try|catch|throw|null|trace|new|this|super|untyped|cast|callback|here)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    ]

enumdefbody' :: TokenMatcher
enumdefbody' =
    [ anyOf whitespace'
    , tok "(?:[a-zA-Z_][a-zA-Z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Instance")
    , tokNext "\\(" (Arbitrary "Punctuation") (GoTo funargdecl')
    , tok ";" (Arbitrary "Punctuation")
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

typedefprebody' :: TokenMatcher
typedefprebody' =
    [ anyOf whitespace'
    , tokNext "(=)(\\s*)({)" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Punctuation")]) (DoAll [Pop, (GoTo typedefbody')])
    ]

classdef' :: TokenMatcher
classdef' =
    [ tokNext "class" (Arbitrary "Keyword" :. Arbitrary "Declaration") (DoAll [(GoTo classdefprebody'), (GoTo typedecl')])
    ]

generictypedecl' :: TokenMatcher
generictypedecl' =
    [ anyOf whitespace'
    , tok "(?:(?:[a-z0-9_\\.])*[A-Z_][A-Za-z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Class")
    , tokNext "<" (Arbitrary "Punctuation") Push
    , tokNext ">" (Arbitrary "Punctuation") Pop
    , tok "," (Arbitrary "Punctuation")
    ]

instancevardecl' :: TokenMatcher
instancevardecl' =
    [ anyOf vardecl'
    , anyOf propertydef'
    ]

classdefprebody' :: TokenMatcher
classdefprebody' =
    [ anyOf whitespace'
    , tokNext "(extends|implements)" (Arbitrary "Keyword" :. Arbitrary "Declaration") (GoTo typedecl')
    , tokNext "{" (Arbitrary "Punctuation") (DoAll [Pop, (GoTo classdefbody')])
    ]

vardef' :: TokenMatcher
vardef' =
    [ tokNext "\\b(var)(\\s+)((?:[a-zA-Z_][a-zA-Z0-9_]*))" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable")]) (GoTo vardecl')
    ]

propertydef' :: TokenMatcher
propertydef' =
    [ tok "(\\()((?:default|null|never))(,)((?:default|null|never))(\\))" (ByGroups [(Arbitrary "Punctuation"), (Arbitrary "Keyword" :. Arbitrary "Reserved"), (Arbitrary "Punctuation"), (Arbitrary "Keyword" :. Arbitrary "Reserved"), (Arbitrary "Punctuation")])
    ]

codeblock' :: TokenMatcher
codeblock' =
    [ anyOf whitespace'
    , anyOf new'
    , anyOf case'
    , anyOf anonfundef'
    , anyOf literals'
    , anyOf vardef'
    , anyOf codekeywords'
    , tok "[();,\\[\\]]" (Arbitrary "Punctuation")
    , tok "(?:=|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|<<=|>>=|>>>=|\\|\\||&&|\\.\\.\\.|==|!=|>|<|>=|<=|\\||&|\\^|<<|>>|>>>|\\+|\\-|\\*|/|%|!|\\+\\+|\\-\\-|\126|\\.|\\?|\\:)" (Arbitrary "Operator")
    , tok "(?:[a-zA-Z_][a-zA-Z0-9_]*)" (Arbitrary "Name")
    , tokNext "}" (Arbitrary "Punctuation") Pop
    , tokNext "{" (Arbitrary "Punctuation") Push
    ]

case' :: TokenMatcher
case' =
    [ tokNext "\\b(case)(\\s+)((?:[a-zA-Z_][a-zA-Z0-9_]*))(\\s*)(\\()" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Reserved"), (Arbitrary "Text"), (Arbitrary "Name"), (Arbitrary "Text"), (Arbitrary "Punctuation")]) (GoTo funargdecl')
    ]

enumdefprebody' :: TokenMatcher
enumdefprebody' =
    [ anyOf whitespace'
    , tokNext "{" (Arbitrary "Punctuation") (DoAll [Pop, (GoTo enumdefbody')])
    ]

typedef' :: TokenMatcher
typedef' =
    [ tokNext "typedef" (Arbitrary "Keyword" :. Arbitrary "Declaration") (DoAll [(GoTo typedefprebody'), (GoTo typedecl')])
    ]

instancevardef' :: TokenMatcher
instancevardef' =
    [ tok "(?:public|private|override|static|inline|extern|dynamic)" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tokNext "\\b(var)(\\s+)((?:[a-zA-Z_][a-zA-Z0-9_]*))" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Declaration"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable" :. Arbitrary "Instance")]) (GoTo instancevardecl')
    ]

literals' :: TokenMatcher
literals' =
    [ tok "0[xX][0-9a-fA-F]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "\126/([^\\n])*?/[gisx]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "\\b(true|false|null)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ anyOf comments'
    , tok "\\s+" (Arbitrary "Text")
    ]

interfacedefprebody' :: TokenMatcher
interfacedefprebody' =
    [ anyOf whitespace'
    , tokNext "(extends)" (Arbitrary "Keyword" :. Arbitrary "Declaration") (GoTo typedecl')
    , tokNext "{" (Arbitrary "Punctuation") (DoAll [Pop, (GoTo classdefbody')])
    ]

type' :: TokenMatcher
type' =
    [ anyOf whitespace'
    , tok "(?:(?:[a-z0-9_\\.])*[A-Z_][A-Za-z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Class")
    , tokNext "<" (Arbitrary "Punctuation") (GoTo generictypedecl')
    , tok "->" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tokNext "(?=[{}(),;=])" (Arbitrary "Text") Pop
    ]

vardecl' :: TokenMatcher
vardecl' =
    [ anyOf whitespace'
    , anyOf typelabel'
    , tokNext "=" (Arbitrary "Operator") Pop
    , tokNext ";" (Arbitrary "Punctuation") Pop
    ]

comments' :: TokenMatcher
comments' =
    [ tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "#[^\\n]*" (Arbitrary "Comment" :. Arbitrary "Preproc")
    ]

enumdef' :: TokenMatcher
enumdef' =
    [ tokNext "enum" (Arbitrary "Keyword" :. Arbitrary "Declaration") (DoAll [(GoTo enumdefprebody'), (GoTo typedecl')])
    ]

imports' :: TokenMatcher
imports' =
    [ tok "(package|import|using)(\\s+)([^;]+)(;)" (ByGroups [(Arbitrary "Keyword" :. Arbitrary "Namespace"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Namespace"), (Arbitrary "Punctuation")])
    ]

new' :: TokenMatcher
new' =
    [ tokNext "\\bnew\\b" (Arbitrary "Keyword") (GoTo typedecl')
    ]

typelabel' :: TokenMatcher
typelabel' =
    [ tokNext ":" (Arbitrary "Punctuation") (GoTo type')
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , anyOf comments'
    , tok "(?:public|private|override|static|inline|extern|dynamic)" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , anyOf enumdef'
    , anyOf typedef'
    , anyOf classdef'
    , anyOf imports'
    ]

funargdecl' :: TokenMatcher
funargdecl' =
    [ anyOf whitespace'
    , tok "(?:[a-zA-Z_][a-zA-Z0-9_]*)" (Arbitrary "Name" :. Arbitrary "Variable")
    , anyOf typelabel'
    , anyOf literals'
    , tok "=" (Arbitrary "Operator")
    , tok "," (Arbitrary "Punctuation")
    , tok "\\?" (Arbitrary "Punctuation")
    , tokNext "\\)" (Arbitrary "Punctuation") Pop
    ]

