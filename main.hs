-- {-# LANGUAGE OverloadedStrings #-}
-- import qualified Data.ByteString.Char8 as S8
-- import qualified Data.Yaml             as Yaml
-- import qualified Network.HTTP.Simple   as Net
-- import           Data.Aeson            (Value)
import Graph

-- get :: IO()
-- get = do
--     response <- Net.httpJSON "https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,
-- +Mountain+View,+CA&key=INSERTAPIKEY"

--     putStrLn $ "The status code was: " ++
--                show (Net.getResponseStatusCode response)
--     print $ Net.getResponseHeader "Content-Type" response
--     S8.putStrLn $ Yaml.encode (Net.getResponseBody response :: Value)

-- waypointUBC :: IO ([Node [Char] Double Double], Double)
-- waypointUBC =
--     do
--         putStrLn "test2"
--         let (NewGraph nodes edges) = buildWeightedGraph [("Musqueam Welcome Post", 15.0, 39.2), ("Main Mall", 2.4, 5.7), ("Brock Hall", 1.0, 1.0)]
--         let nodeNames = getNodeNames nodes
--         line <- ask ("You've selected the Waypoint UBC Map! Select from the following tour destinations:\n" ++ nodeNames ++ "\nExample Input: [\"Main Mall\",\"Brock Hall\"]\n> ")
--         return ([(NewNode "test" 1.0 1.0)], 1.0)

-- Gets the name of all the given nodes
getNameofNodes :: [(Node [Char] Double Double)] -> [Char]
getNameofNodes ((NewNode name1 x1 y1):t) = foldr (\ (NewNode name2 x2 y2) acc -> name2 ++ "|" ++ acc) [] ((NewNode name1 x1 y1):t)

-- Gets a node given the name
getNodeofNames :: [Char] -> [(Node [Char] Double Double)] -> (Node [Char] Double Double)
getNodeofNames str ((NewNode name1 x1 y1):t) = foldr (\ (NewNode name2 x2 y2) acc -> if (str==name2) then (NewNode name2 x2 y2) else acc) (NewNode "error" 0 0) ((NewNode name1 x1 y1):t)

ask q =
    do
        putStr q
        line <- getLine
        return line

-- Introduction to IO, builds graph, welcomes user, and fetches possible destinations. User then selects their first destination, and they enter the recursive navigation loop.
waypoint :: IO [Char]
waypoint =
    do
        let (NewGraph nodes edges) = buildWeightedGraph [("Musqueam Welcome Post", 49.265646111631696, -123.25041373987455), ("Main Mall", 49.26482348106154, -123.25277587971838), ("Brock Hall", 49.268883898466314, -123.25201564267893), ("UBC Aquatic Centre", 49.26782641555527, -123.24874922684376), ("The Nest", 49.26640483638247, -123.24926326761386), ("Orchard Commons Residence", 49.260174961420255, -123.25094001971785), ("Nitobe Memorial Garden", 49.2665725529049, -123.25959303959296), ("UBC Rose Garden", 49.26955940896691, -123.25648431676166)]
        let nodeNames = getNameofNodes nodes
        line <- ask ("Welcome to the Waypoint UBC Map! Where would you like to start your journey:\n" ++ nodeNames ++ "\nExample Input: \"Main Mall\"\n[Type stop at anytime to quit]\n> ")
        if (line=="stop") then 
            return "Thank you for traveling with Waypoint!"
            else do
                let currNode = getNodeofNames line nodes
                putStr ("\nYou have arrived at " ++ line ++ "!")
                let (NewNode name x1 x2) = closest currNode (NewGraph nodes edges)
                putStr ("\nYour next closest Waypoint is" ++ name ++ "!")
                let result = (nextWaypoint (NewGraph nodes edges))
                return result

-- IO Loop that handles navigation to allow repeated journeying to next chosen node, and suggests closest node as an option. Gives option to stop at any time.
nextWaypoint :: (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double), Double)]) -> IO [Char]    
nextWaypoint (NewGraph nodes edges) =
    do
        let nodeNames = getNameofNodes nodes
        line <- ask ("Where would you like to continue your journey:\n" ++ nodeNames ++ "\n[Type stop at anytime to quit]\n> ")
        if (line=="stop") then 
            return ("You have decided to:" ++ line ++ ". Thank you for travelling with Waypoint!")
            else do
                let currNode = getNodeofNames line nodes
                putStr ("\nYou have arrived at " ++ line ++ "!")
                let (NewNode name x1 x2) = closest currNode (NewGraph nodes edges)
                putStr ("\nYour next closest Waypoint is" ++ name ++ "!")
                return (nextWaypoint (NewGraph nodes edges))


-- -- Example Input of Nodes and Edges:
-- -- startup [3,1,2] [9,8,7]
-- startup :: n -> e -> (Graph n e)
-- startup n e = NewGraph n e

-- Main entry point to program
go :: IO [Char]
go = waypoint