import Data.List

type NodeId = Integer
type NodeLabel = String
type EdgeLength = Int
type Distance = Int
type PreviousNode = Maybe Node
    
data Node = Node NodeId NodeLabel deriving (Show,Eq)
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


    
-- Test Data
node1 = Node 1 "a"
node2 = Node 2 "b"
node3 = Node 3 "c"
node4 = Node 4 "d"
        
        
allNodes = [node1,node2,node3,node4]
        
edge12 = Edge node1 node2 10
edge13 = Edge node1 node3 30
edge24 = Edge node2 node4 40
edge34 = Edge node3 node4 5
edge23 = Edge node2 node3 7

allEdges = [edge12,edge13,edge24,edge34,edge23]

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
        
-- Node functions

isStartNode :: Node -> Bool
isStartNode node =
    if 1 == getNodeId node
    then True
    else False
    
getNodeLabel :: Node -> NodeLabel
getNodeLabel (Node _ label ) = label

getNodeId :: Node -> NodeId
getNodeId (Node id _ ) = id


-- Edge functions
findEdges :: [Edge] -> Node -> [Edge]
findEdges allEdges node = 
    filter (\x -> (startNode x) == node) allEdges

findNextNodes :: Node -> [Node]
findNextNodes startNode  =
    map endNode $ findEdges allEdges startNode
           
findNearestNode :: [Edge] -> Node -> Node
findNearestNode allEdges node  =
    endNode $ minimum $ findEdges allEdges node 
               
findShortestDistanceNode :: [PathInfo] -> Node
findShortestDistanceNode paths =
    node $ minimum paths
         
                               
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

updateAllPathInfo :: [PathInfo] -> Node -> [Node] ->[PathInfo]
updateAllPathInfo paths node = undefined
                               

mainLogic :: [PathInfo] -> [Node] -> [PathInfo]
mainLogic paths q  =
    if length q == 0
    then
        paths
    else
        mainLogic newPaths newQ 
        where
          u = findShortestDistanceNode paths
          newQ = delete u allNodes
          connectedNodes = findNextNodes u
          newPaths = updateAllPathInfo paths u connectedNodes

    
main :: IO()
main = do
  putStrLn "Dijkstra Method"
