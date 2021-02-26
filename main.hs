import Graph


-- Example Input of Nodes and Edges:
-- startup [3,1,2] [9,8,7]
startup :: n -> e -> (Graph n e)
startup n e = NewGraph n e
