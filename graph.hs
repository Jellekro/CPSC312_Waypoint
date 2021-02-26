module Graph where

data Graph nodes edges = NewGraph nodes edges
                       deriving Show

insertNode :: Eq name => (Node name x y) -> (Graph [(Node name x y)] edges) -> (Graph [(Node name x y)] edges)
insertNode node (NewGraph nodes edges)
    | elemG node nodes = NewGraph nodes edges
    | otherwise = NewGraph (node:nodes) edges

elemG :: Eq name => (Node name x y) -> [(Node name x y)] -> Bool
elemG (NewNode name x y) [] = False
elemG (NewNode name x y) (h:t) = foldr (\ (NewNode name2 x2 y2) acc -> if (name==name2) then True else acc) False (h:t)

--adds edge containing tuple of node 1 and node 2 as well as the distance between them,
--only if the edge does not already exist in the graph
--addEdge :: (Node name x y) -> (Node name x y) -> (Graph [(Node name x y)] edges) -> (Graph [(Node name x y)] edges)

--given an address, constructs a Node including that address and its lat/lon coordinates
data Node name x y = NewNode name x y
                   deriving Show
--TODO: replace "name x y" with "addr lat lon" and implement construction