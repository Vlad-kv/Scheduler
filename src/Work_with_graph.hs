{-# LANGUAGE RecordWildCards #-}

module Work_with_graph
    ( Graph(..)
    , graphFromEdges
    , graphFromExperiment
    , findCycle
    , TopologicalQueue(..)
    , createTopologicalQueue
    , tqTakeFreeNode
    , tqAcceptNode
    , tqFreezeNode
    , tqIsStopped
    ) where

import Project_structure

import Data.Text (Text, pack)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.State

import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as N

import qualified Data.List as L

import Data.Foldable(fold)

newtype Graph = Graph (Map NodeId [NodeId]) deriving (Eq, Ord)

instance Show Graph where -- debug
    show (Graph g) = iter 1
        where
            iter :: Integer -> String
            iter next = case M.lookup (NodeId next) g of
                            Just val -> show val ++ iter (next + 1)
                            _        -> ""

graphFromEdges :: Integer -> [Edge] -> Graph
graphFromEdges nodesSize edges = Graph $ foldl addEdge (initMap nodesSize) edges
    where
        initMap :: Integer -> Map NodeId [NodeId]
        initMap size = M.fromList $ map (\val -> (NodeId val, [])) [1..size]

        addEdge :: Map NodeId [NodeId] -> Edge -> Map NodeId [NodeId]
        addEdge m e = M.adjust (\l -> edgeDest e : l) (edgeSrc e) m

graphFromExperiment :: Experiment -> Graph
graphFromExperiment exp = graphFromEdges (toInteger $ M.size $ experimentNodes exp) $ experimentEdges exp

findCycle :: Graph -> Maybe (NonEmpty NodeId)
findCycle (Graph g) = let res = evalStateT (foldl
                                                (\st id -> st >> (locFindCycle $ id :| []))
                                                (put M.empty) $
                                                map NodeId [1..(toInteger $ M.size g)])
                                           M.empty
                        in case res of
                            Left l -> Just l
                            _      -> Nothing
    where
        locFindCycle :: NonEmpty NodeId -> StateT (Map NodeId Bool) (Either (NonEmpty NodeId)) ()
        locFindCycle (cur :| t) = do
            res <- gets $ M.lookup cur
            case res of
                Nothing -> (foldl
                                (\st id -> st >> (locFindCycle $ id :| (cur : t)))
                                (modify $ M.insert cur True)
                                (g M.! cur)
                            ) >> (modify $ M.insert cur False)
                Just True -> lift $ Left $ cur :| (reverse $ takeWhile (/= cur) t)
                Just False -> return ()

-- TODO Добавить проверки и возврат ошибок.

data TopologicalQueue = TopologicalQueue
    { tqGraph           :: Graph
    , tqNodesScores     :: Map NodeId Integer
    , tqQueue           :: [NodeId]
    , tqNumNodesToWait  :: Int
    , tqNumBlockedNodes :: Int
    } deriving (Eq, Show)

createTopologicalQueue :: Graph -> TopologicalQueue
createTopologicalQueue (Graph g) =
    TopologicalQueue
        { tqGraph           = Graph g
        , tqNodesScores     = scores
        , tqQueue           = queue
        , tqNumNodesToWait  = 0
        , tqNumBlockedNodes = length queue
        }
    where
        scores :: Map NodeId Integer
        scores = foldl (\scores id -> M.adjust (+ 1) id scores) initialNodesScores (fold $ M.elems g)

        queue :: [NodeId]
        queue = M.keys $ M.filter ( == 0) scores

        initialNodesScores :: Map NodeId Integer
        initialNodesScores = M.fromList $ map (\val -> (NodeId val, 0)) [1..(toInteger $ M.size g)]

tqTakeFreeNode :: TopologicalQueue -> (Maybe NodeId, TopologicalQueue)
tqTakeFreeNode tq@(TopologicalQueue{..}) =
    case tqQueue of
        [] -> (Nothing, tq)
        h : t -> (return h, tq {
                tqQueue = t
              , tqNumNodesToWait = tqNumNodesToWait + 1
            })

tqAcceptNode :: NodeId -> TopologicalQueue -> TopologicalQueue
tqAcceptNode id tq@(TopologicalQueue{ tqGraph = Graph graph, ..}) =
    let (scores, newFreeNodes) = foldl updScores (tqNodesScores, []) (graph M.! id) in
    tq {
        tqQueue = newFreeNodes ++ tqQueue
      , tqNodesScores = scores
      , tqNumNodesToWait = tqNumNodesToWait - 1
      , tqNumBlockedNodes = tqNumBlockedNodes - length newFreeNodes
    }
    where
        updScores :: (Map NodeId Integer, [NodeId]) -> NodeId -> (Map NodeId Integer, [NodeId])
        updScores (scores, newFreeNodes) id =
            ( M.adjust (subtract 1) id scores
            , case scores M.! id of
                1 -> id : newFreeNodes
                _ -> newFreeNodes
            )

tqFreezeNode :: NodeId -> TopologicalQueue -> TopologicalQueue
tqFreezeNode _ tq = tq{ tqNumNodesToWait = (tqNumNodesToWait tq) - 1 }

tqIsStopped :: TopologicalQueue -> Bool
tqIsStopped TopologicalQueue{..} = (tqNumNodesToWait == 0) && (length tqQueue == 0)
