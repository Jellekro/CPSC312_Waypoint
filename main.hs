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

-- Gets the name of all the given nodes
getNameofNodes :: [(Node [Char] Double Double)] -> [Char]
getNameofNodes [] = []
getNameofNodes ((NewNode name1 x1 y1):t) = foldr (\ (NewNode name2 x2 y2) acc -> name2 ++ "\n" ++ acc) [] ((NewNode name1 x1 y1):t)

-- Gets a node given the name
getNodeofNames :: [Char] -> [(Node [Char] Double Double)] -> (Node [Char] Double Double)
getNodeofNames str [] = (NewNode "error" 0 0)
getNodeofNames str ((NewNode name1 x1 y1):t) = foldr (\ (NewNode name2 x2 y2) acc -> if (str==name2) then (NewNode name2 x2 y2) else acc) (NewNode "error" 0 0) ((NewNode name1 x1 y1):t)

-- Gets the total distance of an ordered list of nodes
totalDistance :: [(Node [Char] Double Double)] -> Double
totalDistance [] = 0
totalDistance [a] = 0
totalDistance (node1:node2:t) = (getWeight node1 node2) + totalDistance (node2:t)

ask q =
    do
        putStr q
        line <- getLine
        return line

{-- Introduction via IO, builds graph, welcomes user, and fetches possible destinations. 
 User then selects their first destination, and they enter the recursive navigation loop.
 Tracks user pathing and returns path with total distance upon exit. --}
waypoint :: [(Node [Char] Double Double)] -> IO [Char]
waypoint path =
    do
        let (NewGraph nodes edges) = buildWeightedGraph [("Musqueam Welcome Post", 49.265646111631696, -123.25041373987455), ("Main Mall", 49.26482348106154, -123.25277587971838), ("Brock Hall", 49.268883898466314, -123.25201564267893), ("UBC Aquatic Centre", 49.26782641555527, -123.24874922684376), ("The Nest", 49.26640483638247, -123.24926326761386), ("Orchard Commons Residence", 49.260174961420255, -123.25094001971785), ("Nitobe Memorial Garden", 49.2665725529049, -123.25959303959296), ("UBC Rose Garden", 49.26955940896691, -123.25648431676166)]
        let nodeNames = getNameofNodes nodes
        line <- ask ("Select the next Waypoint on your journey:\n\n" ++ nodeNames ++ "\nExample Input: \"Main Mall\"\n[Type stop at anytime to quit]\n> ")
        if (line=="stop")
        then do
            putStr ("\nTour Path:\n" ++ (getNameofNodes path) ++ "\nTotal Coordinate Distance: " ++ (show (totalDistance path)) ++ "\n\n")
            return "You have decided to stop. Thank you for traveling with Waypoint!"
        else do
            let (NewNode currName currX currY) = getNodeofNames line nodes
            if (currName == "error")
                then do
                    putStr ("\nYou have entered an invalid Waypoint, please try again.\n")
                    waypoint path
                else do
                    putStr ("\nYou have arrived at " ++ line ++ "!")
                    let (NewNode name x1 x2) = closest (NewNode currName currX currY) (NewGraph nodes edges)
                    putStr ("\nYour next closest Waypoint is: " ++ name ++ "!\n\n")
                    waypoint (path ++ [(NewNode currName currX currY)])

-- Main entry point to program
go :: IO [Char]
go =
  do
    putStr ("\n | Welcome to the Waypoint UBC Map! |\n")
    waypoint []
