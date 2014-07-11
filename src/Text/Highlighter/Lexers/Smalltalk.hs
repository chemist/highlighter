module Text.Highlighter.Lexers.Smalltalk (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Smalltalk"
    , lAliases = ["smalltalk", "squeak"]
    , lExtensions = [".st"]
    , lMimetypes = ["text/x-smalltalk"]
    , lStart = root'
    , lFlags = [multiline]
    }

objects' :: TokenMatcher
objects' =
    [ tokNext "\\[" (Arbitrary "Text") (GoTo blockvariables')
    , tokNext "\\]" (Arbitrary "Text") (GoTo afterobject')
    , tokNext "\\b(self|super|true|false|nil|thisContext)\\b" (Arbitrary "Name" :. Arbitrary "Builtin" :. Arbitrary "Pseudo") (GoTo afterobject')
    , tokNext "\\b[A-Z]\\w*(?!:)\\b" (Arbitrary "Name" :. Arbitrary "Class") (GoTo afterobject')
    , tokNext "\\b[a-z]\\w*(?!:)\\b" (Arbitrary "Name" :. Arbitrary "Variable") (GoTo afterobject')
    , tokNext "#(\"[^\"]*\"|[-+*/\\\\\126<>=|&!?,@%]+|[\\w:]+)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol") (GoTo afterobject')
    , anyOf literals'
    ]

blockvariables' :: TokenMatcher
blockvariables' =
    [ anyOf whitespaces'
    , tok "(:)(\\s*)([A-Za-z\\w]+)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable")])
    , tokNext "\\|" (Arbitrary "Operator") Pop
    , tokNext "" (Arbitrary "Text") Pop
    ]

_parenth_helper' :: TokenMatcher
_parenth_helper' =
    [ anyOf whitespaces'
    , tok "(\\d+r)?-?\\d+(\\.\\d+)?(e-?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number")
    , tok "[-+*/\\\\\126<>=|&#!?,@%\\w+:]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "\\'[^\\']*\\'" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "\\$." (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tokNext "#*\\(" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol") (GoTo inner_parenth')
    ]

afterobject' :: TokenMatcher
afterobject' =
    [ tokNext "! !$" (Arbitrary "Keyword") Pop
    , anyOf whitespaces'
    , tokNext "\\b(ifTrue:|ifFalse:|whileTrue:|whileFalse:|timesRepeat:)" (Arbitrary "Name" :. Arbitrary "Builtin") Pop
    , tok "\\b(new\\b(?!:))" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tokNext "\\:=|\\_" (Arbitrary "Operator") Pop
    , tokNext "\\b[a-zA-Z]+\\w*:" (Arbitrary "Name" :. Arbitrary "Function") Pop
    , tok "\\b[a-zA-Z]+\\w*" (Arbitrary "Name" :. Arbitrary "Function")
    , tokNext "\\w+:?|[-+*/\\\\\126<>=|&!?,@%]+" (Arbitrary "Name" :. Arbitrary "Function") Pop
    , tokNext "\\." (Arbitrary "Punctuation") Pop
    , tok ";" (Arbitrary "Punctuation")
    , tok "[\\])}]" (Arbitrary "Text")
    , tokNext "[\\[({]" (Arbitrary "Text") Pop
    ]

literals' :: TokenMatcher
literals' =
    [ tokNext "\\'[^\\']*\\'" (Arbitrary "Literal" :. Arbitrary "String") (GoTo afterobject')
    , tokNext "\\$." (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char") (GoTo afterobject')
    , tokNext "#\\(" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol") (GoTo parenth')
    , tokNext "\\)" (Arbitrary "Text") (GoTo afterobject')
    , tokNext "(\\d+r)?-?\\d+(\\.\\d+)?(e-?\\d+)?" (Arbitrary "Literal" :. Arbitrary "Number") (GoTo afterobject')
    ]

methodDefinition' :: TokenMatcher
methodDefinition' =
    [ tok "([a-zA-Z]+\\w*:)(\\s*)(\\w+)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable")])
    , tok "^(\\b[a-zA-Z]+\\w*\\b)(\\s*)$" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text")])
    , tok "^([-+*/\\\\\126<>=|&!?,@%]+)(\\s*)(\\w+)(\\s*)$" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text"), (Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Text")])
    ]

parenth' :: TokenMatcher
parenth' =
    [ tokNext "\\)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol") (DoAll [(GoTo root'), (GoTo afterobject')])
    , anyOf _parenth_helper'
    ]

inner_parenth' :: TokenMatcher
inner_parenth' =
    [ tokNext "\\)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol") Pop
    , anyOf _parenth_helper'
    ]

whitespaces' :: TokenMatcher
whitespaces' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "\"[^\"]*\"" (Arbitrary "Comment")
    ]

squeakFileout' :: TokenMatcher
squeakFileout' =
    [ tok "^\"[^\"]*\"!" (Arbitrary "Keyword")
    , tok "^'[^']*'!" (Arbitrary "Keyword")
    , tok "^(!)(\\w+)( commentStamp: )(.*?)( prior: .*?!\\n)(.*?)(!)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Keyword")])
    , tok "^(!)(\\w+(?: class)?)( methodsFor: )(\\'[^\\']*\\')(.*?!)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Keyword")])
    , tok "^(\\w+)( subclass: )(#\\w+)(\\s+instanceVariableNames: )(.*?)(\\s+classVariableNames: )(.*?)(\\s+poolDictionaries: )(.*?)(\\s+category: )(.*?)(!)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Keyword")])
    , tok "^(\\w+(?: class)?)(\\s+instanceVariableNames: )(.*?)(!)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Class"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Keyword")])
    , tok "(!\\n)(\\].*)(! !)$" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Keyword")])
    , tok "! !$" (Arbitrary "Keyword")
    ]

root' :: TokenMatcher
root' =
    [ tok "(<)(\\w+:)(.*?)(>)" (ByGroups [(Arbitrary "Text"), (Arbitrary "Keyword"), (Arbitrary "Text"), (Arbitrary "Text")])
    , anyOf squeakFileout'
    , anyOf whitespaces'
    , anyOf methodDefinition'
    , tok "(\\|)([\\w\\s]*)(\\|)" (ByGroups [(Arbitrary "Operator"), (Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Operator")])
    , anyOf objects'
    , tok "\\^|\\:=|\\_" (Arbitrary "Operator")
    , tok "[\\]({}.;!]" (Arbitrary "Text")
    ]

