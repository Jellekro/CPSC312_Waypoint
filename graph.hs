module Graph where

-- Graph ADT Field Definitions
data Graph nodes edges = NewGraph nodes edges
                       deriving Show

-- takes a node and a graph and returns the graph with the node inserted, if it wasn't already
insertNode :: (Eq c) => (Node [c] Double Double) -> (Graph [(Node [c] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))]) -> (Graph [(Node [c] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))])
insertNode node (NewGraph nodes edges)
    | elemNodes node nodes = NewGraph nodes edges
    | otherwise = NewGraph (node:nodes) edges

-- checks if the input node belongs to the input node list
elemNodes :: (Eq c) => (Node [c] Double Double) -> [(Node [c] Double Double)] -> Bool
elemNodes _ [] = False
elemNodes (NewNode name x y) (h:t) = foldr (\ (NewNode name2 x2 y2) acc -> if (name==name2) then True else acc) False (h:t)

-- translates input node information into node objects in the graph
buildNodes :: [([Char], Double, Double)] -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))]) -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))])
buildNodes ((c, d1, d2):t) (NewGraph nodes edges) = foldr (\ n acc -> (insertNode n acc)) (NewGraph nodes edges) [(NewNode c d1 d2) | (c, d1, d2) <- ((c, d1, d2):t)]

-- adds edges between all the nodes in a graph
buildEdges :: (Eq c) => (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))]) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))])
buildEdges (NewGraph nodes edges) = foldr (\ n acc -> (buildEdgesForNode n acc)) (NewGraph nodes edges) nodes

-- adds edges between a node and all nodes but itself
buildEdgesForNode :: (Eq c) => (Node [c] Double Double) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))]) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))])
buildEdgesForNode (NewNode name x y) (NewGraph nodes edges) = foldr (\ n acc -> (insertEdge (NewNode name x y) n acc)) (NewGraph nodes edges) nodes

-- adds an edge between the two input nodes to the input graph, it it wasn't already added
insertEdge :: (Eq c) => (Node [c] Double Double) -> (Node [c] Double Double) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))]) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))])
insertEdge (NewNode name1 x1 y1) (NewNode name2 x2 y2) (NewGraph nodes edges)
    | elemEdges ((NewNode name1 x1 y1),(NewNode name2 x2 y2)) edges = NewGraph nodes edges
    | otherwise = NewGraph nodes (((NewNode name1 x1 y1),(NewNode name2 x2 y2)):edges)

-- checks if the input edge belongs to the input edge list
elemEdges :: (Eq c) => ((Node [c] Double Double),(Node [c] Double Double)) -> [((Node [c] Double Double),(Node [c] Double Double))] -> Bool
elemEdges ((NewNode name1 x1 y1),(NewNode name2 x2 y2)) [] = (name1 == name2)
elemEdges ((NewNode name1 x1 y1),(NewNode name2 x2 y2)) (h:t) = foldr (\ ((NewNode n1 a1 b1),(NewNode n2 a2 b2)) acc -> if (((name1==n1) && (name2==n2)) || ((name1==n2) && (name2==n1)) || (name1 == name2))  then True else acc) False (h:t)

-- translates input node info to a built up graph containing all the given nodes and edges between all nodes
buildGraph :: [([Char], Double, Double)] -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))])
buildGraph ((c, d1, d2):t) = buildEdges (buildNodes ((c, d1, d2):t) (NewGraph [] []))

-- adds weights to the graph's edges
buildWeightedGraph :: [([Char], Double, Double)] -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double), Double)])
buildWeightedGraph ((c, d1, d2):t) = buildWeights (buildGraph ((c, d1, d2):t))

-- iterates through the edges and calls insertWeight
buildWeights :: (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))]) -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double), Double)])
buildWeights (NewGraph nodes edges) = foldr (\ e (NewGraph n wedges) -> (NewGraph n ((insertWeight e):wedges))) (NewGraph nodes []) edges

-- inserts a weight for a given edge
insertWeight :: ((Node [Char] Double Double), (Node [Char] Double Double)) -> ((Node [Char] Double Double), (Node [Char] Double Double), Double)
insertWeight ((NewNode name1 x1 y1),(NewNode name2 x2 y2)) = ((NewNode name1 x1 y1),(NewNode name2 x2 y2), (dist x1 y1 x2 y2))

-- computes distance between two nodes
dist :: Double -> Double -> Double -> Double -> Double
dist x1 y1 x2 y2 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

-- gets the weight for a pair of nodes
getWeight :: (Node [Char] Double Double) -> (Node [Char] Double Double) -> Double
getWeight (NewNode name1 x1 y1) (NewNode name2 x2 y2) = dist x1 y1 x2 y2

-- finds the closest node to the input node, that isn't itself
closest :: (Node [Char] Double Double) -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double), Double)]) -> (Node [Char] Double Double)
closest (NewNode name1 x1 y1) (NewGraph nodes edges) = foldr (\ (NewNode name2 x2 y2) acc -> if ((name1/=name2) && ((getWeight (NewNode name1 x1 y1) (NewNode name2 x2 y2))<(getWeight (NewNode name1 x1 y1) acc))) then (NewNode name2 x2 y2) else acc) (NewNode "error" 2147483647 2147483647) nodes

--given an address, constructs a Node including that address and its lat/lon coordinates
data Node name x y = NewNode name x y
                   deriving Show