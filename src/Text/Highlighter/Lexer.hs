module Text.Highlighter.Lexer (runLexer) where

import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State (State, gets, modify, evalState)
import Text.Regex.PCRE.Light hiding (compile)
import Text.Regex.PCRE.Light.Char8 (compile)
import qualified Data.ByteString as BS
import Data.Sequence (Seq, empty, singleton, (><), viewl, null, ViewL(..))
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Data.Foldable (toList, foldr1, mapM_)
import Prelude hiding (lex, foldr1, mapM_, concat, head, drop, tail, reverse, dropWhile, null)
import qualified Prelude as P

import Text.Highlighter.Types


data LexerState =
    LexerState
        { lsLexer :: Lexer
        , lsInput :: BS.ByteString
        , lsState :: [TokenMatcher]
        , lsLexed :: (Seq Token)
        , lastNotNull :: Bool
        }
    deriving Show

type LexerM = ExceptT LexerError (State LexerState)

data LexerError
    = NoMatchFor BS.ByteString
    | OtherLexerError String
    deriving Show

runLexer :: Lexer -> BS.ByteString -> Either LexerError [Token]
runLexer l s =  toList <$> runLexer' l s

runLexer' :: Lexer -> BS.ByteString -> Either LexerError (Seq Token)
runLexer' l s = evalState (runExceptT lex) (LexerState l s [lStart l] empty True)

lex :: LexerM (Seq Token)
lex = do
    done <- gets (BS.null . lsInput)

    if done
        then gets lsLexed
        else do

    ms <- getState
    ts <- tryAll ms
    if null ts || (BS.null . tText . head $ ts)
       then modify $ \ls -> ls { lsLexed = lsLexed ls >< ts }
       else modify $ \ls -> ls { lsLexed = lsLexed ls >< ts 
                              , lastNotNull = (BS.last . tText . head $ ts) == 10
                              }
    lex
  where
    getState = gets (P.head . lsState)

isBOL :: LexerM Bool
isBOL = gets lastNotNull

head :: Seq a -> a
head x = let (b :< _) =  viewl x
         in b

tryAll :: [Match] -> LexerM (Seq Token)
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
            return empty

        Just (s:ss) -> do
            modify $ \ls -> ls { lsInput = BS.drop (BS.length s) i }

            nextState (mNextState m) (s:ss)

            toTokens (s:ss) (mType m)

        Nothing ->
            tryAll ms `catchError` trySkipping
  where
    trySkipping (NoMatchFor _) = tryAllFirst (m:ms)
    trySkipping e = throwError e

tryAllFirst :: [Match] -> LexerM (Seq Token)
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
            return . singleton . Token Error $ (skipped <> (tText $ head ts))

        _ -> tryAllFirst ms

toTokens :: [BS.ByteString] -> TokenType -> LexerM (Seq Token)
toTokens (s:_) (Using l) = either throwError return (runLexer' l s)
toTokens (_:ss) (ByGroups ts) = foldr1 (><) <$> mapM (\(s,t) -> toTokens [s] t) (P.zip ss ts)
toTokens (s:_) t = return $ singleton $ Token t s
toTokens [] _ = return empty

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
    modify $ \ls -> ls { lsState = P.tail (lsState ls) }
nextState (PopNum n) _ =
    modify $ \ls -> ls { lsState = P.drop n (lsState ls) }
nextState Push _ =
    modify $ \ls -> ls { lsState = P.head (lsState ls) : lsState ls }
nextState (GoTo n) _ =
    modify $ \ls -> ls { lsState = n : lsState ls }
nextState (CapturesTo f) cs =
    modify $ \ls -> ls { lsState = f (map fromBS cs) : lsState ls }
  where
    fromBS = map (toEnum . fromEnum) . BS.unpack
nextState (DoAll nss) cs = mapM_ (flip nextState cs) nss
nextState (Combined nss) _ =
    modify $ \ls -> ls { lsState = P.concat nss : lsState ls }
