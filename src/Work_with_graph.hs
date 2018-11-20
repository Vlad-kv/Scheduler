module Work_with_graph
    ( Graph(..)
    , graphFromEdges
    , graphFromExperiment
    , findCycle
    )where

import Project_structure

import Data.Text (Text, pack)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.Trans.State

import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as N

import Control.Monad.Trans.Class(lift)

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
        addEdge m e = M.update (\l -> Just $ edgeDest e : l) (edgeSrc e) m

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
