module Graph where

data Graph nodes edges = NewGraph nodes edges
                       deriving Show

insertNode :: (Eq c) => (Node [c] Double Double) -> (Graph [(Node [c] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))]) -> (Graph [(Node [c] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))])
insertNode node (NewGraph nodes edges)
    | elemNodes node nodes = NewGraph nodes edges
    | otherwise = NewGraph (node:nodes) edges

elemNodes :: (Eq c) => (Node [c] Double Double) -> [(Node [c] Double Double)] -> Bool
elemNodes _ [] = False
elemNodes (NewNode name x y) (h:t) = foldr (\ (NewNode name2 x2 y2) acc -> if (name==name2) then True else acc) False (h:t)

buildNodes :: [([Char], Double, Double)] -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))]) -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))])
buildNodes ((c, d1, d2):t) (NewGraph nodes edges) = foldr (\ n acc -> (insertNode n acc)) (NewGraph nodes edges) [(NewNode c d1 d2) | (c, d1, d2) <- ((c, d1, d2):t)]

--adds edge containing tuple of node 1 and node 2 as well as the distance between them,
--only if the edge does not already exist in the graph
buildEdges :: (Eq c) => (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))]) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))])
buildEdges (NewGraph nodes edges) = foldr (\ n acc -> (buildEdgesForNode n acc)) (NewGraph nodes edges) nodes

buildEdgesForNode :: (Eq c) => (Node [c] Double Double) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))]) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))])
buildEdgesForNode (NewNode name x y) (NewGraph nodes edges) = foldr (\ n acc -> (insertEdge (NewNode name x y) n acc)) (NewGraph nodes edges) nodes

insertEdge :: (Eq c) => (Node [c] Double Double) -> (Node [c] Double Double) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))]) -> (Graph [(Node [c] Double Double)] [((Node [c] Double Double), (Node [c] Double Double))])
insertEdge (NewNode name1 x1 y1) (NewNode name2 x2 y2) (NewGraph nodes edges)
    | elemEdges ((NewNode name1 x1 y1),(NewNode name2 x2 y2)) edges = NewGraph nodes edges
    | otherwise = NewGraph nodes (((NewNode name1 x1 y1),(NewNode name2 x2 y2)):edges)

elemEdges :: (Eq c) => ((Node [c] Double Double),(Node [c] Double Double)) -> [((Node [c] Double Double),(Node [c] Double Double))] -> Bool
elemEdges ((NewNode name1 x1 y1),(NewNode name2 x2 y2)) [] = (name1 == name2)
elemEdges ((NewNode name1 x1 y1),(NewNode name2 x2 y2)) (h:t) = foldr (\ ((NewNode n1 a1 b1),(NewNode n2 a2 b2)) acc -> if (((name1==n1) && (name2==n2)) || ((name1==n2) && (name2==n1)) || (name1 == name2))  then True else acc) False (h:t)

buildGraph :: [([Char], Double, Double)] -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))])
buildGraph ((c, d1, d2):t) = buildEdges (buildNodes ((c, d1, d2):t) (NewGraph [] []))

buildWeightedGraph :: [([Char], Double, Double)] -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double), Double)])
buildWeightedGraph ((c, d1, d2):t) = buildWeights (buildGraph ((c, d1, d2):t))

buildWeights :: (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double))]) -> (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double), Double)])
buildWeights (NewGraph nodes edges) = foldr (\ e (NewGraph n wedges) -> (NewGraph n ((insertWeight e):wedges))) (NewGraph nodes []) edges

insertWeight :: ((Node [Char] Double Double), (Node [Char] Double Double)) -> ((Node [Char] Double Double), (Node [Char] Double Double), Double)
insertWeight ((NewNode name1 x1 y1),(NewNode name2 x2 y2)) = ((NewNode name1 x1 y1),(NewNode name2 x2 y2), (dist x1 y1 x2 y2))

dist :: Double -> Double -> Double -> Double -> Double
dist x1 y1 x2 y2 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)


--given an address, constructs a Node including that address and its lat/lon coordinates
data Node name x y = NewNode name x y
                   deriving Show
--TODO: replace "name x y" with "addr lat lon" and implement construction