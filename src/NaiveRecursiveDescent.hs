{-# LANGUAGE MultiParamTypeClasses #-}
module NaiveRecursiveDescent where

import           Pre

import qualified Control.Monad.State as State
-- import qualified Data.Vector         as Vector
import qualified Data.Set.Monad      as Set

type Position = Natural

data ParseError token = UnMatchedToken
                        { expectedToken :: token
                        , actualToken   ::  token}
                      | InvalidPosition Natural
                      deriving (Show, Eq, Ord)

newtype ParseErrors token = ParseErrors { unParseErrors :: Set (ParseError token) }
                          deriving (Show, Eq)

singleton :: Ord token => ParseError token -> ParseErrors token
singleton err = ParseErrors $ Set.singleton err

instance Semigroup (ParseErrors token) where
  x <> y = ParseErrors $ unParseErrors x <> unParseErrors y


data ParserCurrentState token = ParserCurrentState
                                { pcsSrc      :: Vector token
                                , pcsPosition :: Position
                                , pcsErrors   :: ParseErrors token
                                } deriving (Show, Eq)

newtype Parser token a = Parser { pState :: State (ParserCurrentState token) (Maybe a) }

instance Functor (Parser token) where
  fmap f Parser { pState = sM } = Parser { pState = fmap f <$> sM }

instance Applicative (Parser token) where
  pure x = Parser { pState = pure $ Just x }
  Parser { pState = sM0 } <*> Parser { pState = sM1 } =
    Parser { pState = (<*>) <$> sM0 <*> sM1 }

instance Alternative (Parser token) where
  empty = undefined
  Parser { pState = sM0 } <|> Parser { pState = sM1 } =
    Parser
    { pState = do
        s     <- State.get
        mayX0 <- sM0
        case mayX0 of
             Nothing -> do
               State.put s
               sM1
             _       -> pure mayX0
    }

instance Monad (Parser token) where
   Parser { pState = sM } >>= f =
     Parser
     { pState = do
         s <- State.get
         mayX <- sM
         case mayX of
              Nothing -> do
                State.put s
                pure Nothing
              Just x  -> pState $ f x
     }

instance State.MonadState (ParserCurrentState token) (Parser token) where
  get = Parser { pState = State.gets Just }
  put x = Parser { pState = Just <$> State.put x }
  state f = Parser { pState = Just <$> State.state f }

instance Ord token => MonadError (ParseErrors token) (Parser token) where
  throwError err = do
    State.modify $ \pcs ->
      pcs { pcsErrors = err <> pcsErrors pcs }
    Parser { pState = pure Nothing }

  catchError failableM handler =
    Parser
    { pState = do
        mayX <- pState failableM
        case mayX of
          Nothing -> State.gets pcsErrors
                     >>= pState . handler
          _       -> pure mayX
    }

terminal :: Ord token => token -> Parser token token
terminal tok = do
  ParserCurrentState
    { pcsSrc = src
    , pcsPosition = pos } <- State.get
  case src !? fromIntegral pos of
    Just x | tok == x  -> stepPure x
           | otherwise -> throwError $ singleton $
                          UnMatchedToken
                          {expectedToken = tok
                          , actualToken = x}
    Nothing -> throwError $ singleton $ InvalidPosition pos

stepPure :: a -> Parser token a
stepPure x = do
  State.modify $ \s ->
    s { pcsPosition = 1 + pcsPosition s }
  pure x
