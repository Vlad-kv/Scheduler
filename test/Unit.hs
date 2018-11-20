module Unit
    ( hspecTestTree
    ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

import Project_structure
import Work_with_graph

import Data.Map (Map)
import qualified Data.Map as M

import Data.List.NonEmpty(NonEmpty(..))

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "Sheduler" tests

graphFromList :: Integer -> [(Integer, Integer)] -> Graph
graphFromList n edges = graphFromEdges n $ map
            (\(src, dest) -> Edge
                            { edgeSrc  = NodeId src
                            , edgeDest = NodeId dest})
            edges

isValidCycle :: Graph -> Maybe (NonEmpty NodeId) -> Bool
isValidCycle _ Nothing = False
isValidCycle (Graph g) (Just (h :| t)) = validateAllEdges $ h : t ++ [h]
    where
        validateAllEdges :: [NodeId] -> Bool
        validateAllEdges []            = True
        validateAllEdges [h]           = True
        validateAllEdges (h1 : h2 : t) = (validateEdge h1 h2) && (validateAllEdges $ h2 : t)

        validateEdge :: NodeId -> NodeId -> Bool
        validateEdge src dest = case M.lookup src g of
                                    Just l -> elem dest l
                                    _      -> False

tests :: Spec
tests = do
    describe "Cycles detection" $ do
        it "Empty graph" $
            let g = graphFromList 7 [] in
            findCycle g `shouldBe` Nothing
        it "Full acyclic graph" $
            let g = graphFromList 4 [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)] in
            findCycle g `shouldBe` Nothing
        it "Full graph" $
            let g = graphFromList 3 [(1, 2), (1, 3), (2, 3), (2, 1), (3, 1), (3, 2)] in
            findCycle g `shouldSatisfy` isValidCycle g
        it "eyelet" $
            let g = graphFromList 1 [(1, 1)] in
            findCycle g `shouldBe` Just ((NodeId 1) :| [])
        it "Almost empty graph" $
            let g = graphFromList 400 [(125, 314), (314, 40), (40, 202), (202, 125)] in
            findCycle g `shouldSatisfy` isValidCycle g
        it "Mixed graph 1" $
            let g = graphFromList 7 [(1, 2), (3, 2), (3, 4), (5, 4), (5, 6), (6, 1)] in
            findCycle g `shouldBe` Nothing
        it "Mixed graph 2" $
            let g = graphFromList 10 [(1, 2), (3, 2), (3, 1), (4, 5), (5, 6), (5, 1), (6, 10), (10, 3), (10, 4)] in
            findCycle g `shouldSatisfy` isValidCycle g
    describe "Test 2" $ do
        it "test 2.1" $
            (1 + 5 - 2) `shouldBe` (10 - 6)
