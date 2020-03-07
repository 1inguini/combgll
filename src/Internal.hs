{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Internal where

-- import           Pre

-- import qualified Control.Monad.State                as State
-- import qualified Data.Map.Strict                    as Map
-- import qualified Data.Set                           as DefaultSet (Set, toList)
-- import qualified Data.Set.Monad                     as Set
-- import qualified Data.Vector                        as Vector
-- import qualified Data.Vector.Generic.Mutable        as MVector

-- import qualified Algebra.Graph.Acyclic.AdjacencyMap as DAG
-- import qualified Algebra.Graph.AdjacencyMap         as Graph

-- type DAG = DAG.AdjacencyMap

-- fromDefaultSet :: Ord a => DefaultSet.Set a -> Set a
-- fromDefaultSet = Set.fromList . DefaultSet.toList

-- -- R in the paper "GLL Parsing" is a set of current descripters
-- -- Ui = {(L, u) | (L, u, i) ∈ things that have been added to R}.

-- data RecognizerLabel lexeme = Top
--                             | End
--                             | Terminal lexeme
--                             | Alternate (RecognizerLabel lexeme) (RecognizerLabel lexeme)
--                             deriving (Show, Eq, Ord)

-- type Position = Natural

-- type GSS a = Set [a]

-- data Node lexeme = Node
--                    { nodePosition   :: Position
--                    , nodeRecognizer :: Recognizer lexeme
--                    }
--                  deriving (Show, Eq, Ord)


-- data Descripter lexeme = Descripter
--                          { descRecognizer :: Recognizer lexeme
--                          , descNode       :: Node lexeme
--                          , descPosition   :: Position
--                          }
--                        deriving (Show, Eq, Ord)

-- data RecognizerInternalState lexeme = RecognizerInternalState
--                                { rsSource      :: Vector lexeme
--                                , rsPosition    :: Position
--                                , rsDescripters :: Set (Descripter lexeme)
--                                , rsGSS         :: DAG (Node lexeme)
--                                , rsAdded       :: Vector (Set (RecognizerLabel lexeme, Node lexeme))
--                                , rsPopped      :: Set (Node lexeme, Position)
--                                , rsCurrentNode :: Node lexeme
--                                -- , psNewestLabel :: Label
--                                } deriving (Show, Eq)

-- data Recognizer lexeme = Recognizer { rcLabel :: RecognizerLabel lexeme
--                                     , rcState :: State (RecognizerInternalState lexeme) () }

-- type RecognizerState lexeme = State (RecognizerInternalState lexeme) ()

-- -- type Recognizer lexeme = Recognizer' lexeme ()

-- instance (Show lexeme) => Show (Recognizer lexeme) where
--   show Recognizer { rcLabel = l } = show l

-- instance (Eq lexeme) => Eq (Recognizer lexeme) where
--    Recognizer { rcLabel = l0 } == Recognizer { rcLabel = l1 } = l0 == l1

-- instance (Ord lexeme) => Ord (Recognizer lexeme) where
--    compare Recognizer { rcLabel = l0 } Recognizer { rcLabel = l1 } = compare l0 l1

-- instance Semigroup (Recognizer lexeme) where
--   Recognizer { rcLabel = l0, rcState = s0 } <> Recognizer { rcLabel = l1, rcState = s1 } =
--     Recognizer { rcLabel = Alternate l0 l1
--                , rcState = s0 *> s1 }

-- -- instance Functor (Recognizer' lexeme) where
-- --   fmap f rc = rc { rcState = f <$> rcState rc }

-- -- instance Applicative (Recognizer' lexeme) where
-- --   pure x = Recognizer { rcLabel = Pured
-- --                       , rcState = pure x }
-- --   Recognizer { rcLabel = l0, rcState = s0 } <*> Recognizer { rcLabel = l1, rcState = s1 } =
-- --     Recognizer { rcLabel = l0 <> l1
-- --                , rcState = s0 <*> s1 }

-- -- instance Monad (Recognizer' lexeme) where
-- --    Recognizer { rcLabel = l, rcState = s } >>= f =
-- --      Recognizer
-- --      { rcLabel = l
-- --      , rcState = do
-- --          a <- s
-- --          let Recognizer { rcState = s' } = f a
-- --          s'
-- --      }

-- -- instance State.MonadState (RecognizerState lexeme) (Recognizer' lexeme) where
-- --   get = Recognizer { rcLabel = Zero, rcState = State.get }
-- --   put x = Recognizer { rcLabel = Zero, rcState = State.put x }
-- --   state f = Recognizer { rcLabel = Zero, rcState = State.state f }

-- -- anonymousRecongizer :: State (RecognizerState lexeme) () -> Recognizer lexeme
-- -- anonymousRecongizer body = Recognizer { rcLabel = Zero, rcState = body }

-- node0 :: Node lexeme
-- node0 = undefined

-- vectorModifyAt :: (Integral i) => i -> Vector a -> (a -> a) -> Vector a
-- vectorModifyAt i vec modifier = Vector.modify
--   (\v -> MVector.modify v modifier (fromIntegral i)) vec

-- add :: Ord lexeme => Recognizer lexeme -> Node lexeme -> Position -> RecognizerState lexeme
-- add rc@Recognizer { rcLabel = l } node position = do
--   RecognizerInternalState
--     { rsAdded = added
--     } <- State.get
--   case not . Set.member (l, node) <$> added !? fromIntegral position of
--     (Just True) -> State.modify $ \rs ->
--       rs { rsDescripters = Set.insert
--                            (Descripter
--                              { descRecognizer = rc
--                              , descNode       = node
--                              , descPosition   = position })
--                            $ rsDescripters rs
--          , rsAdded       = vectorModifyAt position (rsAdded rs)
--                            (Set.insert (l, node)) }
--     _ -> pure ()

-- pop :: (Ord lexeme) => Node lexeme -> RecognizerState lexeme
-- pop node@Node { nodeRecognizer = l } = if node /= node0
--   then do
--   position <- State.gets rsPosition
--   State.modify $ \rs ->
--     rs { rsPopped = Set.insert (node, position)
--                    $ rsPopped rs }
--   gss <- State.gets rsGSS
--   (\v -> add l v position) `mapM_` fromDefaultSet (DAG.postSet node gss)
--   else pure ()

-- create :: Ord lexeme => Recognizer lexeme -> Node lexeme -> RecognizerState lexeme
-- create rc node = do
--   RecognizerInternalState
--     { rsPosition = position
--     , rsGSS      = gss
--     , rsPopped   = popped
--     } <- State.get
--   let v = Node { nodeRecognizer = rc, nodePosition = position}
--   if DAG.hasEdge v node gss
--     then do
--     State.modify $ \rs ->
--                      rs { rsGSS = DAG.shrink $ Graph.overlay
--                                   (Graph.edge v node)
--                                   (DAG.fromAcyclic gss)
--                         }
--     let poppedVposition = do
--           (n, p) <- popped
--           guard (n /= node)
--           pure p
--     add rc node `mapM_` poppedVposition
--     else pure ()

-- terminal :: Ord lexeme => lexeme -> Recognizer lexeme
-- terminal lexeme = (\s -> Recognizer { rcLabel = Terminal lexeme, rcState = s }) $ do
--   RecognizerInternalState
--     { rsSource   = src
--     , rsPosition = srcPos
--     } <- State.get
--   case src !? fromIntegral srcPos of
--     Just x | x == lexeme -> step
--     _                    -> rcState topRecognizer

-- step :: RecognizerState lexeme
-- step = do
--   State.modify $ \rs ->
--     rs { rsPosition = 1 + rsPosition rs }
--   pure ()

-- topRecognizer :: Recognizer lexeme
-- topRecognizer = undefined

-- -- initialNode :: Node
-- -- initialNode = Node { nodeLabel    = Numbered 1
-- --                    , nodePosition = 0 }

-- -- endNode :: Integral a => a -> Node
-- -- endNode srcLength = Node{ nodeLabel     = Numbered 0
-- --                         , nodePosition  = 1 + fromIntegral srcLength }


-- -- emptyRecognizerState :: RecognizerState lexeme
-- -- emptyRecognizerState = RecognizerState
-- --                         { rsSource            = mempty
-- --                         , rsPosition          = 0
-- --                         , rsDescripters       = mempty
-- --                         , rsGSS               = DAG.empty
-- --                         , rsAdded             = mempty
-- --                         , rsPopped            = mempty
-- --                         , rsCurrentNode       = initialNode
-- --                         , rsNamedRecognizers = mempty
-- --                         }

-- -- initialRecognizerState :: Vector lexeme -> RecognizerState lexeme
-- -- initialRecognizerState src =
-- --   let srcLength = Vector.length src
-- --   in emptyRecognizerState
-- --      { rsSource = src
-- --      , rsGSS    = DAG.shrink $ Graph.edge
-- --                   (endNode srcLength)
-- --                   initialNode }

-- runRecognizer :: Recognizer lexeme -> RecognizerInternalState lexeme -> Bool
-- runRecognizer = undefined
-- -- runRecognizer rc@Recognizer { rcLabel = label, rcState = state } rs =
-- --   let resultState@RecognizerState
-- --         { rsDescripters = resultDescripters
-- --         , rsSource      = resultSource
-- --         } = State.execState state rs
-- --       srcLength = Vector.length resultSource
-- --   in if Set.null resultDescripters
-- --   then maybe False (Set.member (Numbered 0, endNode srcLength)) $
-- --        rsAdded resultState !? srcLength
-- --   else let ( Descripter { descLabel    = l
-- --                         , descNode     = node
-- --                         , descPosition = position },
-- --              descripters ) =
-- --              Set.deleteFindMin resultDescripters
-- --     in runRecognizer rc resultState { rsDescripters = descripters
-- --                                      , rsCurrentNode = node
-- --                                      , rsPosition    = position }


-- -- -- S ::= ASd | B S | ε
-- -- -- A ::= a | c
-- -- -- B ::= a | b

-- -- -- lS :: (Label, Recognizer Text)
-- -- -- lS = (LabelID "lS"
-- -- --      ,
-- -- --      )
