import Data.List

type NodeId = Integer
type NodeLabel = String
type EdgeLength = Int
type Distance = Int
type PreviousNode = Maybe Node
    
data Node = Node NodeId NodeLabel deriving (Show,Eq)
data Edge = Edge Node Node EdgeLength deriving (Show,Eq)

-- Data structure to record shortest distance and path info
data PathInfo = PathInfo {
              node :: Node,
              prevNode :: PreviousNode,
              distance :: Distance }
              deriving (Show,Eq)

                       
instance Ord Edge where
    compare (Edge _ _ len) (Edge _ _ len')
        | len == len'   = EQ
        | len < len'    = LT
        | otherwise = GT


getDistance :: [PathInfo] -> Node -> Distance
getDistance paths node1 = 
            distance $ head $ filter (\x -> (node x) == node1) paths
            
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

-- Init Path info

initPathInfo :: [PathInfo]
initPathInfo =
    map (\node -> if isStartNode node
                  then PathInfo node Nothing 0
                  else PathInfo node Nothing (maxBound :: Int)
    )
    allNodes

allPath = initPathInfo
 
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
getEdgeLength :: Edge -> EdgeLength
getEdgeLength (Edge _ _ len) = len

getStartNode :: Edge -> Node
getStartNode (Edge start _ _) = start

getEndNode :: Edge -> Node
getEndNode (Edge _ end _) = end

findEdges :: [Edge] -> Node -> [Edge]
findEdges allEdges node = 
    filter (\x -> (getStartNode x) == node) allEdges

findNextNodes :: [Edge] -> Node -> [Node]
findNextNodes allEdges startNode  =
    map getEndNode $ findEdges allEdges startNode
           
findNearestNode :: [Edge] -> Node -> Node
findNearestNode allEdges node  =
    getEndNode $ minimum $ findEdges allEdges node 
               
findShortestDistanceNode =
    undefined
                                      
mainLogic :: [Node] -> [Node]
mainLogic allNodes =
    newQ
    where
      u = findShortestDistanceNode allNodes
      newQ = delete u allNodes
      nextNodeList = findNextNodes allEdges u


-- NodeとNodeを結ぶEdgeを求める
-- Node間は直結されている必要がある
findEdge :: [Edge] -> Node -> Node -> Edge
findEdge allEdges n1 n2 =
    head $ filter (\x -> (getEndNode x) == n2) $ findEdges allEdges n1

    


main :: IO()
main = do
  putStrLn "Dijkstra Method"
