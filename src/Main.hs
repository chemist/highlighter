import Prelude hiding (head)

import Text.Highlighter
import Text.Highlighter.Formatters.Html

import System.Environment
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.String
import qualified Data.ByteString as BS

main :: IO ()
main = do
    as <- getArgs

    case as of
        [fn] -> do
            case lexerFromFilename fn of
                Just l -> do
                    s <- BS.readFile fn
                    case runLexer l s of
                        Right ts ->
                            putStrLn . renderHtml . mainPage . format True $ ts

                        Left e ->
                            error (show e)
                Nothing -> error "unknown file type"

        _ -> putStrLn "usage: hspygments FILENAME"

mainPage :: Html -> Html
mainPage h =
    docTypeHtml $ do
        head $
            link ! rel "stylesheet" ! href "highlight.css"

        body h
