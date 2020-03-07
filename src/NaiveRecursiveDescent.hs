{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
module NaiveRecursiveDescent where

import           Pre

import qualified Control.Monad.State.Strict as State
-- import qualified Data.Vector         as Vector
import qualified Data.Char                  as Char
import qualified Data.Text                  as Text
import           Data.Sequence              ((<|), (|>))
import qualified Data.Sequence              as Seq
import qualified Data.Set.Monad             as Set

import qualified Data.Text.Read as Text

type Position = Natural

class Ord token => Parseable tokens token | tokens -> token, token -> tokens where
  emptySrc    :: tokens
  (!?)        :: Integral i => tokens -> i -> Maybe token
  endPosition :: tokens -> Position
  takeChunk   :: (Position, Position) -> Parse tokens token tokens
  
  
data ParseWhat token = ParseNothing
                     | Terminal token
                     | Or (ParseWhat token) (ParseWhat token)
                     | Sequence (Seq (ParseWhat token))
                     | Description Text
                     deriving (Show, Eq, Ord)

instance Semigroup (ParseWhat token) where
  (<>) ParseNothing    pl              = pl
  (<>) pl              ParseNothing    = pl
  (<>) (Sequence pls0) (Sequence pls1) = Sequence $ pls0 <> pls1
  (<>) pl              (Sequence pls)  = Sequence $ pl <| pls
  (<>) (Sequence pls)  pl              = Sequence $ pls |> pl
  (<>) pl0             pl1             = Sequence $ Seq.fromList [pl0, pl1]

instance Monoid (ParseWhat token) where
  mempty = ParseNothing

data ParseError token = UnExpectedToken
                      | InvalidPosition
                      deriving (Show, Eq, Ord)

data ParseErrorPos token = ParseError
                           { errorPosition :: Position
                           , failedParser  :: ParseWhat token
                           , parseError    :: ParseError token
                           } deriving (Show, Eq, Ord)

newtype ParseErrors token = ParseErrors { unParseErrors :: Set (ParseErrorPos token) }
                          deriving (Show, Eq)

singleError :: Ord token => Position -> ParseWhat token -> ParseError token -> ParseErrors token
singleError pos parser err = ParseErrors $ Set.singleton ParseError
                  { errorPosition = pos
                  , failedParser  = parser
                  , parseError    = err }

instance Semigroup (ParseErrors token) where
  x <> y = ParseErrors $ unParseErrors x <> unParseErrors y

instance Ord token => Monoid (ParseErrors token) where
  mempty = ParseErrors mempty

data ParserCurrentState tokens token =
  ParserCurrentState
  { pcsSrc           :: tokens
  , pcsPosition      :: Position
  , pcsErrors        :: ParseErrors token
  , pcsCurrentParser :: ParseWhat token
  } deriving (Show, Eq)

emptyPCS :: Parseable tokens token => ParserCurrentState tokens token
emptyPCS = ParserCurrentState
           { pcsSrc           = emptySrc
           , pcsPosition      = 0
           , pcsErrors        = mempty
           , pcsCurrentParser = ParseNothing
           }

newtype PStateRet token a = Result { psrMayResult :: Maybe a }
                          deriving (Show, Eq)

psrNothing :: PStateRet token a
psrNothing = Result
             { -- psrParseWhat = ParseNothing
             -- ,
               psrMayResult = Nothing }

instance Functor (PStateRet token) where
  fmap f psr@Result { psrMayResult = mayX } = psr { psrMayResult = f <$> mayX }

instance Applicative (PStateRet token) where
  pure x = Result { psrMayResult = Just x }
  Result { psrMayResult = mayX0 } <*> Result { psrMayResult = mayX1 } =
    Result { psrMayResult = mayX0 <*> mayX1 }

instance Monad (PStateRet token) where
   Result { psrMayResult = mayX } >>= f =
     Result { psrMayResult = mayX >>= psrMayResult . f }

newtype Parse tokens token a =
  Parser { pState :: State (ParserCurrentState tokens token) (PStateRet token a) }

getCurrentParser :: State (ParserCurrentState tokens token) (ParseWhat token)
getCurrentParser = State.gets pcsCurrentParser

putCurrentParser :: ParseWhat token -> State (ParserCurrentState tokens token) ()
putCurrentParser parseWhat = State.modify $ \pcs ->
    pcs { pcsCurrentParser = parseWhat }

appendToCurrentParser :: ParseWhat token -> State (ParserCurrentState tokens token) ()
appendToCurrentParser parseWhat = do
  previousParseWhat <- getCurrentParser
  putCurrentParser $ previousParseWhat <> parseWhat

-- getParseWhat :: Ord token => Parse token a -> ParseWhat token
-- getParseWhat Parser { pState = sM } =
--   psrParseWhat $ State.evalState sM emptyPCS

instance Functor (Parse tokens token) where
  fmap f ps@Parser { pState = sM } = ps { pState = fmap f <$> sM }

instance Applicative (Parse tokens token) where
  pure x = Parser { pState = do
                      State.modify $ \pcs ->
                        pcs { pcsCurrentParser = ParseNothing }
                      pure $ pure x }
  Parser { pState = sM0 } <*> Parser { pState = sM1 } =
    Parser
    { pState = do
        mayX0 <- sM0
        p0    <- getCurrentParser
        mayX1 <- sM1
        p1    <- getCurrentParser
        State.modify $ \pcs ->
          pcs { pcsCurrentParser = p0 <> p1 }
        pure $ mayX0 <*> mayX1
    }

instance Ord token => Alternative (Parse tokens token) where
  empty = Parser { pState = pure psrNothing }
  Parser { pState = sM0 } <|> Parser { pState = sM1 } =
    Parser
    { pState = do
        initialState <- State.get
        Result { psrMayResult = mayX0 } <- sM0
        parseWhat0 <- getCurrentParser
        result <- case mayX0 of
          Nothing -> do
            State.put initialState
            sM1
          _       ->
            pure Result { psrMayResult = mayX0 }
        parseWhat1 <- getCurrentParser
        putCurrentParser $ Or parseWhat0 parseWhat1
        pure result
    }

instance Ord token => Monad (Parse tokens token) where
   Parser { pState = sM } >>= f =
     Parser
     { pState = do
         initialState <- State.get
         Result { psrMayResult = mayX } <- sM
         case mayX of
           Nothing -> do
             State.put initialState
             pState $ throwSingleError UnExpectedToken
           Just x  -> do
             parseWhat <- getCurrentParser
             Result { psrMayResult = mayX' } <- pState $ f x
             parseWhat' <- getCurrentParser
             putCurrentParser $ parseWhat <> parseWhat'
             pure Result { psrMayResult = mayX' }
     }

instance Ord token => State.MonadState (ParserCurrentState tokens token) (Parse tokens token) where
  get = Parser { pState = State.gets pure }
  put x = Parser { pState = pure <$> State.put x }
  state f = Parser { pState = pure <$> State.state f }

instance Ord token => MonadError (ParseErrors token) (Parse tokens token) where
  throwError errs = do
    State.modify $ \pcs ->
      pcs { pcsErrors = errs <> pcsErrors pcs }
    Parser { pState = pure psrNothing }

  catchError failableM handler =
    Parser
    { pState = do
        initialState <- State.get
        rs@Result { psrMayResult = mayX } <- pState failableM
        case mayX of
          Nothing -> do
            errs <- State.gets pcsErrors
            State.put initialState
            Result { psrMayResult = handlerMayX } <- pState $ handler errs
            pure Result { psrMayResult = handlerMayX }
          _       -> pure rs
    }

throwSingleError :: Ord token => ParseError token -> Parse tokens token a
throwSingleError err = do
  ParserCurrentState
    { pcsPosition      = pos
    , pcsCurrentParser = parseWhat } <- State.get
  throwError $ singleError pos parseWhat err

stepPure :: Ord token => a -> Parse tokens token a
stepPure x = do
  State.modify $ \s ->
    s { pcsPosition = 1 + pcsPosition s }
  pure x

giveName :: ParseWhat token -> Parse tokens token a -> Parse tokens token a
giveName parseWhat Parser { pState = sM } = Parser
  { pState = do
      mayX <- sM
      putCurrentParser parseWhat
      pure mayX
  }

label :: Text -> Parse tokens token a -> Parse tokens token a
label txt = giveName (Description txt)

(<?>) :: Parse tokens token a -> Text -> Parse tokens token a
(<?>) = flip label

satisfy :: Parseable tokens token => (token -> Bool) -> Parse tokens token token
satisfy tester = label "satisfy" $ do
  ParserCurrentState
    { pcsSrc = src
    , pcsPosition = pos } <- State.get
  case src !? pos of
    Just x | tester x  -> stepPure x
           | otherwise -> throwSingleError UnExpectedToken
    Nothing            -> throwSingleError InvalidPosition

condtionalStep :: Parseable tokens token => (token -> Bool) -> Parse tokens token Bool
condtionalStep tester' = label "condtionalStep" $ do
  ParserCurrentState
    { pcsSrc = src
    , pcsPosition = pos } <- State.get
  case src !? pos of
    Just x | tester' x  -> stepPure True
    _                   -> pure False

satisfyRegion :: Parseable tokens token => (token -> Bool) -> Parse tokens token (Position, Position)
satisfyRegion tester = label "satisfyRegion" $ do
  start <- State.gets pcsPosition
  loop tester
  end <- State.gets pcsPosition  
  pure (start, end)
  where
    loop :: Parseable tokens token => (token -> Bool) -> Parse tokens token ()
    loop tester' = do
      bool <- condtionalStep tester'
      if bool
        then loop tester'
        else pure ()
    
terminal :: Parseable tokens token => token -> Parse tokens token token
terminal tok = giveName (Terminal tok) $ do
  ParserCurrentState
    { pcsSrc = src
    , pcsPosition = pos } <- State.get
  case src !? pos of
    Just x | tok == x  -> stepPure x
           | otherwise -> throwSingleError UnExpectedToken
    Nothing -> throwSingleError InvalidPosition

eof :: Parseable tokens token => Parse tokens token ()
eof = do
  ParserCurrentState
    { pcsSrc      = src
    , pcsPosition = pos
    } <- State.get
  if pos == endPosition src
    then pure ()
    else throwSingleError UnExpectedToken

runParser :: Parseable tokens token =>
  Parse tokens token a
  -> tokens
  -> Either (ParseErrors token) a
runParser parser src =
  let ( Result { psrMayResult = mayResult },
        ParserCurrentState { pcsErrors = errs }) =
        State.runState (pState parser) emptyPCS { pcsSrc = src }
  in maybe (Left errs) Right mayResult 

type Parser a = Parse Text Char a

instance Parseable Text Char where
  emptySrc = Text.empty
  txt !? i =
    let i' = fromIntegral i
    in if i' < Text.length txt
       then Just $ Text.index txt i'
       else Nothing
  endPosition src = fromIntegral (Text.length src)
  takeChunk (from, to) =
    Text.drop (fromIntegral from)
    . Text.take (fromIntegral to)
    <$> State.gets pcsSrc
    
decimal :: Integral i => Parser i
decimal = label "decimal" $ do
  txt <- satisfyRegion Char.isDigit >>= takeChunk
  pure $ case Text.decimal txt of
    Left _      -> 0
    Right (i,_) -> i

space :: Parser ()
space = label "space" $
  () <$ condtionalStep Char.isSpace
