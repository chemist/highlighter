{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Text.Highlighter.Types where

import Data.Char (toLower)
import Text.Regex.PCRE.Light
import qualified Data.ByteString as BS

data Match
    = Match
        { mRegexp :: String
        , mType :: TokenType
        , mNextState :: NextState
        }
    | AnyOf TokenMatcher
    deriving Show

type TokenMatcher = [Match]

data Lexer =
    Lexer
        { lName :: String
        , lAliases :: [String]
        , lExtensions :: [String]
        , lMimetypes :: [String]
        , lStart :: TokenMatcher
        , lFlags :: [PCREOption]
        }

data NextState
    = Continue
    | GoTo TokenMatcher
    | CapturesTo Callback
    | Pop
    | PopNum Int
    | Push
    | DoAll [NextState]
    | Combined [TokenMatcher]
    deriving Show

type Callback = [String] -> TokenMatcher

data Token =
    Token
        { tType :: TokenType
        , tText :: BS.ByteString
        }
    deriving Show

data TokenType
    = Text
    | Whitespace
    | Error
    | Other
    | Keyword
    | Name
    | Literal
    | String
    | Number
    | Operator
    | Punctuation
    | Comment
    | Generic
    | TokenType :. TokenType

    -- Subtypes
    -- Keyword
    | Declaration
    | Reserved
    | Type

    -- Keyword, Name.Builtin
    | Pseudo

    -- Keyword, Name
    | Namespace

    -- Nane, Name.Variable
    | Class

    -- Keyword, Name
    | Constant

    -- Name
    | Attribute
    | Builtin
    | Decorator
    | Entity
    | Exception
    | Function
    | Identifier
    | Label
    | Property
    | Tag
    | Variable
    | Global
    | Instance
    | Anonymous

    -- Literal
    | Date
    | Scalar
    | Plain

    -- String
    | Atom
    | Backtick
    | Char
    | Doc
    | Double
    | Escape
    | Heredoc
    | Interp
    | Interpol
    | Regex
    | Regexp
    | Symbol

    -- String, Comment
    | Single

    -- Number
    | Bin
    | Binary
    | Decimal
    | Float
    | Hex
    | Hexadecimal
    | Int
    | Integer
    | Long
    | Oct
    | Octal

    -- Operator
    | Word

    -- Comment
    | Multiline
    | Preproc
    | Special

    -- Generic
    | Deleted
    | Emph
    | Heading
    | Inserted
    | Output
    | Prompt
    | Strong
    | Subheading
    | Traceback
    | ByGroups [TokenType]

    | Indicator

    -- Some arbitrary token name
    | Arbitrary String

    -- Use another lexer to yield some tokens
    | Using Lexer
    deriving Show

instance Show Lexer where
    show l = "(Lexer " ++ lName l ++ ")"

instance Show Callback where
    show _ = "Callback"

tok :: String -> TokenType -> Match
tok s tt = Match s tt Continue

tokNext :: String -> TokenType -> NextState -> Match
tokNext s = Match s

anyOf :: TokenMatcher -> Match
anyOf ms = AnyOf ms

shortName :: TokenType -> String
shortName ((_ :. a) :. b) =
    shortName a ++ shortName b
shortName (Name :. Constant) = "no"
shortName (Name :. Entity) = "ni"
shortName (Name :. Property) = "py"
shortName (Arbitrary "Name" :. Arbitrary "Constant") = "no"
shortName (Arbitrary "Name" :. Arbitrary "Entity") = "ni"
shortName (Arbitrary "Name" :. Arbitrary "Property") = "py"
shortName (Literal :. b) =
    shortName b
shortName (Arbitrary "Literal" :. b) =
    shortName b
shortName (a :. Other) =
    shortName a ++ "x"
shortName (a :. Error) =
    shortName a ++ "r"
shortName (a :. Single) =
    shortName a ++ "1"
shortName (a :. Double) =
    shortName a ++ "2"
shortName (a :. Arbitrary "Other") =
    shortName a ++ "x"
shortName (a :. Arbitrary "Error") =
    shortName a ++ "r"
shortName (a :. Arbitrary "Single") =
    shortName a ++ "1"
shortName (a :. Arbitrary "Double") =
    shortName a ++ "2"
shortName (a :. b) =
    shortName a ++ shortName b
shortName Error = "err"
shortName (Arbitrary "Error") = "err"
shortName Number = "m"
shortName (Arbitrary "Number") = "m"
shortName (Arbitrary t) =
    [toLower (head t)]
shortName x = [toLower (head (show x))]
