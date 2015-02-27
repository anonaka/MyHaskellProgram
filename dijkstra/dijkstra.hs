type NodeId = Integer
type NodeLabel = String
type EdgeLength = Integer

data Node = Node NodeId NodeLabel deriving (Show,Eq)
data Edge = Edge Node Node EdgeLength deriving (Show,Eq)

-- Test Data
node1 = Node 1 "a"
node2 = Node 2 "b"
node3 = Node 3 "c"
node4 = Node 4 "d"

edge12 = Edge node1 node2 10
edge13 = Edge node1 node3 30
edge24 = Edge node2 node4 40
edge34 = Edge node3 node4 5
