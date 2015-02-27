type NodeId = Integer
type NodeLabel = String
type EdgeLength = Integer

data Node = Node NodeId NodeLabel
data Edge = Edge Node Node EdgeLength