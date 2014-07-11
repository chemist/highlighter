module Text.Highlighter
    ( module Text.Highlighter.Lexer
    , module Text.Highlighter.Lexers
    , module Text.Highlighter.Types
    , lexerFromFilename
    ) where

import Text.Highlighter.Lexer
import Text.Highlighter.Lexers
import Text.Highlighter.Types

import System.FilePath

lexerFromFilename :: FilePath -> Maybe Lexer
lexerFromFilename = flip lookup lexers . takeExtension
