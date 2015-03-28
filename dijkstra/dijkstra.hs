import Data.List

-- -- Test Data
-- node1 = Node 1 "a"
-- node2 = Node 2 "b"
-- node3 = Node 3 "c"
-- node4 = Node 4 "d"        
        
-- allNodes = [node1,node2,node3,node4]
        
-- edge12 = Edge node1 node2 10
-- edge13 = Edge node1 node3 30
-- edge24 = Edge node2 node4 40
-- edge34 = Edge node3 node4 5
-- edge23 = Edge node2 node3 7

-- allEdges = [edge12,edge13,edge24,edge34,edge23]

-- Data
node1 = Node 1 "s" -- Start
node2 = Node 2 "h"
node3 = Node 3 "u"
node4 = Node 4 "e"
node5 = Node 5 "e"
node6 = Node 6 "n"
node7 = Node 7 "e"
node8 = Node 8 "a"
node9 = Node 9 "o"        
node10 = Node 10 "h"
node11 = Node 11 "j"
node12 = Node 12 "t"
node13 = Node 13 "g" -- Goal
node14 = Node 14 "i"
node15 = Node 15 "h"
node16 = Node 16 "e"        
        
        
allNodes = [node1,node2,node3,node4,node5,node6,node7,node8,node9,
            node10,node11,node12,node13,node14,node15,node16]
        
edge1_2 = Edge node1 node2 1
edge1_4 = Edge node1 node4 3
edge1_11 = Edge node1 node11 5
edge1_14 = Edge node1 node14 16
           
edge2_3 = Edge node2 node3 14
edge2_4 = Edge node2 node4 1
edge2_6 = Edge node2 node6 16

edge3_6 = Edge node3 node6 1
edge3_7 = Edge node3 node7 7

edge4_5 = Edge node4 node5 5
edge4_8 = Edge node4 node8 3

edge5_2 = Edge node5 node2 4
edge5_9 = Edge node5 node9 6

edge6_5 = Edge node6 node5 3
edge6_7 = Edge node6 node7 2 
edge6_9 = Edge node6 node9 2
edge6_10 = Edge node6 node10 6

edge7_10 = Edge node7 node10 5
edge7_13 = Edge node7 node13 17

edge8_5 = Edge node8 node5 1
edge8_14 = Edge node8 node14 3

edge9_12 = Edge node9 node12 1

edge10_13 = Edge node10 node13 10
edge10_16 = Edge node10 node16 3

edge11_4 = Edge node11 node4 1
edge11_8 = Edge node11 node8 3            
edge11_14 = Edge node11 node14 5  

edge12_10 = Edge node12 node10 4
edge12_15 = Edge node12 node15 3

edge14_12 = Edge node14 node12 4
edge14_15 = Edge node14 node15 10
            
edge15_10 = Edge node15 node10 5
edge15_16 = Edge node15 node16 10

edge16_13 = Edge node16 node13 15            
          
allEdges = [
 edge1_2,
 edge1_4,
 edge1_11,
 edge1_14,
           
 edge2_3,
 edge2_4,
 edge2_6,

 edge3_6,
 edge3_7,

 edge4_5,
 edge4_8,

 edge5_2,
 edge5_9,

 edge6_5,
 edge6_7,
 edge6_9,
 edge6_10,

 edge7_10,
 edge7_13,
         
 edge8_5,
 edge8_14,

 edge9_12,

 edge10_13,
 edge10_16,

 edge11_4,
 edge11_8,
 edge11_14,

 edge12_10,
 edge12_15,

 edge14_12,
 edge14_15,
            
 edge15_10,
 edge15_16,

 edge16_13
 ]

-- End Data

type NodeId = Integer
type NodeLabel = String
type EdgeLength = Int
type Distance = Int
type PreviousNode = Maybe Node
    
data Node = Node {
      nodeId :: NodeId,
      nodeLabel :: NodeLabel
    } deriving (Show,Eq)
          
data Edge = Edge {
      startNode :: Node,
      endNode :: Node,
      edgeLength :: EdgeLength
    } deriving (Show,Eq)

-- Data structure to record shortest distance and path info
data PathInfo = PathInfo {
      node :: Node,
      prevNode :: PreviousNode,
      distance :: Distance
    } deriving (Show,Eq)

instance Ord PathInfo where
    compare path path'
        | (distance path) == (distance path') = EQ
        | (distance path) < (distance path') = LT
        | otherwise = GT
               
instance Ord Edge where
    compare edge edge'
        | (edgeLength edge) == (edgeLength edge') = EQ
        | (edgeLength edge) < (edgeLength edge') = LT
        | otherwise = GT
    
-- Init 
initPathInfo :: [PathInfo]
initPathInfo =
    map (\node -> if isStartNode node
                  then PathInfo node Nothing 0
                  else PathInfo node Nothing (maxBound :: Int)
    )
    allNodes

allPaths = initPathInfo
 
findPathInfo :: [PathInfo] -> Node -> PathInfo
findPathInfo allPath nd =
    head $ filter (\x -> nd == (node x)) allPath
        
isStartNode :: Node -> Bool
isStartNode node =
    if 1 == nodeId node
    then True
    else False

findEdges :: [Edge] -> Node -> [Edge]
findEdges allEdges node = 
    filter (\x -> (startNode x) == node)
           allEdges
-- findEdges allEdges node = 
--    filter (\x -> (((startNode x) == node) || ((endNode x) == node)))
--           allEdges

           
findNextNodes :: Node -> [Node]
findNextNodes startNode  =
    map endNode $ findEdges allEdges startNode
           
findShortestDistanceNode :: [PathInfo] -> [Node] -> Node
findShortestDistanceNode paths nodes =
    node $ minimum $ filter (\x -> elem (node x) nodes) paths
                                        
-- NodeとNodeを結ぶEdgeを求める
-- Node間は直結されている必要がある
findEdge :: [Edge] -> Node -> Node -> Edge
findEdge allEdges n1 n2 =
    head $ filter (\x -> (endNode x) == n2) $ findEdges allEdges n1

getLengthBetweenNodes :: Node -> Node -> EdgeLength
getLengthBetweenNodes n1 n2 =
    edgeLength $ findEdge allEdges n1 n2

getDistance :: [PathInfo] -> Node -> Distance
getDistance paths node =
    distance $ findPathInfo paths node

updateDistanceAndPrevNode  :: [PathInfo] -> Node -> Distance -> PreviousNode -> [PathInfo]
updateDistanceAndPrevNode paths node1 distance prevn =
    newPath : oldPaths where
        -- This is very inefficient. Needs improvement.
        oldPath = head $ filter (\x -> (node x) == node1) paths
        oldPaths = filter (\x -> (node x) /= node1) paths
        newPath = PathInfo node1 prevn distance


findShorterPath :: [PathInfo] -> Node -> Node -> [PathInfo]
findShorterPath paths node1 node2 =
    let d1 = getDistance paths node1
        d2 = getDistance paths node2
        len = getLengthBetweenNodes node1 node2
        newDistance = d1 + len
    in
      if d2 > newDistance
      then
          -- update PathInfo with new distance
          updateDistanceAndPrevNode paths node2 newDistance (Just node1)
      else
          paths

updateAllPathInfo :: [PathInfo] -> Node -> [Node] -> [PathInfo]
updateAllPathInfo paths startNode [] = paths
updateAllPathInfo paths startNode (x:xs) =
    let paths' = findShorterPath paths startNode x
    in updateAllPathInfo paths' startNode xs
      
mainLogic :: [PathInfo] -> [Node] -> [PathInfo]
mainLogic paths [] = paths
mainLogic paths q  =
    mainLogic newPaths newQ 
    where
      u = findShortestDistanceNode paths q
      newQ = delete u q
      connectedNodes = findNextNodes u
      newPaths = updateAllPathInfo paths u connectedNodes

showPath :: [PathInfo] -> Node -> [Maybe Node]
showPath paths goalNode  =
    showPathIter paths (Just goalNode) [Just goalNode]

showPathIter :: [PathInfo] -> (Maybe Node) -> [Maybe Node] -> [Maybe Node]
showPathIter paths goalNode result =
    if isStartNodeM goalNode
    then result
    else
        showPathIter paths previousNode (previousNode:result)
        where
          previousNode = findPathInfoM paths goalNode

isStartNodeM :: (Maybe Node) -> Bool
isStartNodeM Nothing = False
isStartNodeM (Just node) = isStartNode node

findPathInfoM :: [PathInfo] -> (Maybe Node) -> (Maybe Node)
findPathInfoM paths Nothing = Nothing
findPathInfoM paths (Just node1) =
    prevNode $ findPathInfo paths node1


ppPath :: [Maybe Node] -> [NodeLabel]               
ppPath [] = []
ppPath (Nothing:_) = []
ppPath ((Just x):xs) = ((nodeLabel x):(ppPath xs))

shortestPath = ppPath $ showPath (mainLogic allPaths allNodes) node13
               
main :: IO()
main = do
  putStrLn "Dijkstra Method"

