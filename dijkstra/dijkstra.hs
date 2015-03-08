type NodeId = Integer
type NodeLabel = String
type EdgeLength = Int
type Distance = Int

data Node = Node NodeId NodeLabel Distance deriving (Show,Eq)
data Edge = Edge Node Node EdgeLength deriving (Show,Eq)

instance Ord Node where
    compare (Node _ _ d) (Node _ _ d')
        | d == d' = EQ
        | d < d' = LT
        | otherwise = GT

instance Ord Edge where
    compare (Edge _ _ len) (Edge _ _ len')
        | len == len'   = EQ
        | len < len'    = LT
        | otherwise = GT


-- Test Data
node1 = Node 1 "a" 0 -- 1st node
node2 = Node 2 "b" (maxBound :: Int)
node3 = Node 3 "c" (maxBound :: Int)
node4 = Node 4 "d" (maxBound :: Int) -- goal node
        
allNodes = [node1,node2,node3,node4]
        
edge12 = Edge node1 node2 10
edge13 = Edge node1 node3 30
edge24 = Edge node2 node4 40
edge34 = Edge node3 node4 5
edge23 = Edge node2 node3 7

allEdges = [edge12,edge13,edge24,edge34,edge23]

-- Node functions

getNodeLabel :: Node -> NodeLabel
getNodeLabel (Node _ label _) = label

getNodeId :: Node -> NodeId
getNodeId (Node id _ _) = id

getNodeDistance :: Node -> Distance
getNodeDistance (Node _  _ distance) = distance

setNodeDistance :: Node -> Distance -> Node
setNodeDistance node distance = Node (getNodeId node)(getNodeLabel node) distance

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
               
findShortestDistanceNode :: [Node] -> Node
findShortestDistanceNode allNodes =
    minimum allNodes
            
-- 
main :: IO()
main = do
  putStrLn "Dijkstra Method"
