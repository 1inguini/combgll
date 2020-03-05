module NaiveRecursiveDescent where

import           Pre

import qualified Control.Monad.State as State
import qualified Data.Vector         as Vector

data ParserCurrentState token = ParserCurrentState
                                { pcsSrc :: Vector token
                                } deriving (Show, Eq)
