module Unit
    ( hspecTestTree
    ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy, testSpec)

import Project_structure
import Work_with_graph

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.List.NonEmpty(NonEmpty(..))

import Data.Foldable(fold)

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

validateOrder :: Graph -> [NodeId] -> Either String ()
validateOrder (Graph g) nodes = 
    let posById = posInList nodes 1 in
    let edges   = fold $ map (\(val, l) -> map (\val' -> (val, val')) l) (M.assocs g) in
    if all (inCorrectOrder posById) edges then return ()
    else Left "Invalid order"

    where
        inCorrectOrder :: (Map NodeId Integer) -> (NodeId, NodeId) -> Bool
        inCorrectOrder posById (id1, id2) = 
            let res = M.lookup id1 posById >>= \pos1 ->
                        M.lookup id2 posById >>= \pos2 ->
                            return (pos1 < pos2)
            in
            case res of
                Just val -> val
                Nothing -> True

        posInList :: [NodeId] -> Integer -> Map NodeId Integer
        posInList (h:nodes) curPos = M.insert h curPos $ posInList nodes (curPos + 1)
        posInList _ _ = M.empty

testExtractingtAllAchievableNodes :: Graph -> [Integer] -> [Integer] -> Either String ()
testExtractingtAllAchievableNodes graph@(Graph g) freezedNodes expectedVisitesNodes =
    let tq = createTopologicalQueue graph in
    let (tq', nodes) = strategy1 (S.fromList $ map NodeId freezedNodes) tq in
    validateTQ tq' >> validateOrder graph nodes >> validateEquality (map NodeId expectedVisitesNodes) nodes

    where
        validateTQ :: TopologicalQueue -> Either String ()
        validateTQ tq = if tqIsStopped tq then return ()
                        else Left $ "TQ is not stopped"

        validateEquality :: [NodeId] -> [NodeId] -> Either String ()
        validateEquality l1 l2 = let diff = S.difference (S.fromList l1) (S.fromList l2) in
                                if S.null diff then Right ()
                                else Left $ "Not empty difference : " ++ (show $ S.toList diff) ++ (show (S.fromList l2))

        -- По порядку.
        strategy1 :: Set NodeId -> TopologicalQueue -> (TopologicalQueue, [NodeId])
        strategy1 freezedNodes tq = case tqTakeFreeNode tq of
                            (Nothing, tq') -> (tq', [])
                            (Just id, tq') ->
                                let (tq'', res) = strategy1 freezedNodes (if S.member id freezedNodes
                                                                            then tqFreezeNode id tq'
                                                                            else tqAcceptNode id tq')
                                in (tq'', id : res)

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
    describe "TopologicalQueue work" $ do
        it "Test good order" $
            let g = graphFromList 4 [(4, 2), (4, 1), (2, 3), (1, 3)] in
            validateOrder g (map NodeId [4, 2, 1, 3]) `shouldBe` Right ()
        it "Test bad order" $
            let g = graphFromList 4 [(4, 2), (4, 1), (2, 3), (1, 3)] in
            validateOrder g (map NodeId [3, 2, 1, 4]) `shouldNotBe` Right ()
        it "Empty graph" $
            let g = graphFromList 0 [] in
            testExtractingtAllAchievableNodes g [] [] `shouldBe` Right ()
        it "Graph without edges" $
            let g = graphFromList 4 [] in
            testExtractingtAllAchievableNodes g [] [1, 2, 3, 4] `shouldBe` Right ()
        it "All nodes achievable" $
            let g = graphFromList 5 [(1, 2), (3, 2), (3, 4), (5, 4)] in
            testExtractingtAllAchievableNodes g [] [1..5] `shouldBe` Right ()
        it "Some nodes freezed" $
            let g = graphFromList 6 [(1, 2), (3, 2), (3, 4), (5, 4), (5, 6)] in
            testExtractingtAllAchievableNodes g [3] [1, 5] `shouldBe` Right ()
        it "All roots freezed" $
            let g = graphFromList 6 [(1, 3), (2, 4), (3, 5), (4, 5)] in
            testExtractingtAllAchievableNodes g [1, 2, 5] [] `shouldBe` Right ()

    describe "Test 2" $ do
        it "test 2.1" $
            (1 + 5 - 2) `shouldBe` (10 - 6)
