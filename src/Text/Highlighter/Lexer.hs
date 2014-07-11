module Text.Highlighter.Lexer where

import Control.Monad.Error
import Control.Monad.State
import Prelude hiding (lex)
import Text.Regex.PCRE.Light hiding (compile)
import Text.Regex.PCRE.Light.Char8 (compile)
import qualified Data.ByteString as BS

import Text.Highlighter.Types


data LexerState =
    LexerState
        { lsLexer :: Lexer
        , lsInput :: BS.ByteString
        , lsState :: [TokenMatcher]
        , lsLexed :: [Token]
        }
    deriving Show

type LexerM = ErrorT LexerError (State LexerState)

data LexerError
    = NoMatchFor BS.ByteString
    | OtherLexerError String
    deriving Show

instance Error LexerError where
    noMsg = OtherLexerError "unknown"
    strMsg = OtherLexerError

runLexer :: Lexer -> BS.ByteString -> Either LexerError [Token]
runLexer l s = evalState (runErrorT lex) (LexerState l s [lStart l] [])

lex :: LexerM [Token]
lex = do
    done <- gets (BS.null . lsInput)

    if done
        then gets lsLexed
        else do

    ms <- getState
    ts <- tryAll ms
    modify $ \ls -> ls { lsLexed = lsLexed ls ++ ts }
    lex
  where
    getState = gets (head . lsState)

tryAll :: [Match] -> LexerM [Token]
tryAll [] = do
    i <- gets lsInput
    throwError (NoMatchFor i)
tryAll (AnyOf ms:ms') =
    tryAll (ms ++ ms')
tryAll (m:ms) = do
    atbol <- isBOL
    fs <- gets (lFlags . lsLexer)

    let opts
            | atbol     = [exec_anchored]
            | otherwise = [exec_anchored, exec_notbol]

    i <- gets lsInput
    case match (compile (mRegexp m) fs) i opts of
        Just [] -> do
            nextState (mNextState m) []
            return []

        Just (s:ss) -> do
            modify $ \ls -> ls { lsInput = BS.drop (BS.length s) i }

            nextState (mNextState m) (s:ss)

            toTokens (s:ss) (mType m)

        Nothing ->
            tryAll ms `catchError` trySkipping
  where
    trySkipping e = do
        case e of
            NoMatchFor _ ->
                tryAllFirst (m:ms)

            _ -> throwError e

tryAllFirst :: [Match] -> LexerM [Token]
tryAllFirst [] = do
    i <- gets lsInput
    throwError (NoMatchFor i)
tryAllFirst (AnyOf ms:ms') =
    tryAllFirst (ms ++ ms')
tryAllFirst (m:ms) = do
    atbol <- isBOL
    fs <- gets (lFlags . lsLexer)

    let opts
            | atbol     = []
            | otherwise = [exec_notbol]

    i <- gets lsInput
    case match (compile (mRegexp m) fs) i opts of
        Just (s:ss) -> do
            let (skipped, next) = skipFailed i s
            modify $ \ls -> ls { lsInput = next }
            ts <- toTokens (s:ss) (mType m)
            return (Token Error skipped:ts)

        _ -> tryAllFirst ms


isBOL :: LexerM Bool
isBOL = do
    ld <- gets lsLexed
    case ld of
        [] -> return True
        ts ->
            let nonempty = dropWhile (BS.null . tText) (reverse ts)
            in
                case nonempty of
                    [] -> return True
                    (t:_) -> return (BS.last (tText t) == 10)

toTokens :: [BS.ByteString] -> TokenType -> LexerM [Token]
toTokens (s:_) (Using l) = do
    either throwError return (runLexer l s)
toTokens (_:ss) (ByGroups ts) =
    liftM concat $ zipWithM (\s t -> toTokens [s] t) ss ts
toTokens (s:_) t = return [Token t s]
toTokens [] _ = return []

-- Given the starting point, return the text preceding and after
-- the failing regexp match
skipFailed :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
skipFailed i r
    | r `BS.isPrefixOf` i = (BS.empty, BS.drop (BS.length r) i)
    | otherwise =
        let (pre, next) = skipFailed (BS.tail i) r
        in (BS.cons (BS.head i) pre, next)

nextState :: NextState -> [BS.ByteString] -> LexerM ()
nextState Continue _ = return ()
nextState Pop _ =
    modify $ \ls -> ls { lsState = tail (lsState ls) }
nextState (PopNum n) _ =
    modify $ \ls -> ls { lsState = drop n (lsState ls) }
nextState Push _ =
    modify $ \ls -> ls { lsState = head (lsState ls) : lsState ls }
nextState (GoTo n) _ =
    modify $ \ls -> ls { lsState = n : lsState ls }
nextState (CapturesTo f) cs =
    modify $ \ls -> ls { lsState = f (map fromBS cs) : lsState ls }
  where
    fromBS = map (toEnum . fromEnum) . BS.unpack
nextState (DoAll nss) cs = mapM_ (flip nextState cs) nss
nextState (Combined nss) _ =
    modify $ \ls -> ls { lsState = concat nss : lsState ls }
