type NodeId = Integer
type NodeLabel = String
type EdgeLength = Integer

data Node = Node NodeId NodeLabel deriving (Show,Eq)
data Edge = Edge Node Node EdgeLength deriving (Show,Eq)

instance Ord Edge where
  compare (Edge n1 n2 len) (Edge n1' n2' len')
    | len == len'   = EQ
    | len < len'    = LT
    | otherwise = GT


-- Test Data
node1 = Node 1 "a"
node2 = Node 2 "b"
node3 = Node 3 "c"
node4 = Node 4 "d"

edge12 = Edge node1 node2 10
edge13 = Edge node1 node3 30
edge24 = Edge node2 node4 40
edge34 = Edge node3 node4 5
edge23 = Edge node2 node3 7

allEdges = [edge12,edge13,edge24,edge34,edge23]

getNodeLabel :: Node -> NodeLabel
getNodeLabel (Node _ label) = label

getNodeId :: Node -> NodeId
getNodeId (Node id _) = id

getEdgeLength :: Edge -> EdgeLength
getEdgeLength (Edge _ _ len) = len

getStartNode :: Edge -> Node
getStartNode (Edge start _ _) = start

getEndNode :: Edge -> Node
getEndNode (Edge _ end _) = end

findEdges :: [Edge] -> Node -> [Edge]
findEdges allEdges node = 
    filter (\x -> (getStartNode x) == node) allEdges

findNearest :: [Edge] -> Node -> Node
findNearest allEdges node  =
    getEndNode $ minimum $ findEdges allEdges node 

main :: IO()
main = do
  putStrLn "Dijkstra Method"
