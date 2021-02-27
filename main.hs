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


getNameofNodes :: [(Node [Char] Double Double)] -> [Char]
getNameofNodes ((NewNode name1 x1 y1):t) = foldr (\ (NewNode name2 x2 y2) acc -> name2 ++ "|" ++ acc) [] ((NewNode name1 x1 y1):t)

getNodeofNames :: [Char] -> [(Node [Char] Double Double)] -> (Node [Char] Double Double)
getNodeofNames str ((NewNode name1 x1 y1):t) = foldr (\ (NewNode name2 x2 y2) acc -> if (str==name2) then (NewNode name2 x2 y2) else acc) (NewNode "error" 0 0) ((NewNode name1 x1 y1):t)

ask q =
    do
        putStr q
        line <- getLine
        return line

waypoint :: IO String
waypoint =
    do
        -- line <- ask ("Welcome to Waypoint! Please choose your destination:\nEnter 1 for UBC, or 2 for Downtown Vancouver\n> ")
        let (NewGraph nodes edges) = buildWeightedGraph [("Musqueam Welcome Post", 15.0, 39.2), ("Main Mall", 2.4, 5.7), ("Brock Hall", 1.0, 1.0)]
        let nodeNames = getNameofNodes nodes
        line <- ask ("Welcome to the Waypoint UBC Map! Where would you like to start your journey:\n" ++ nodeNames ++ "\nExample Input: \"Main Mall\"\n[Type stop at anytime to quit]\n> ")
        if (line=="stop") then 
            return "Thank you for travelling with Waypoint!"
            else do
                let currNode = getNodeofNames line nodes
                putStr ("\nYou have arrived at " ++ line ++ "!")
                let (NewNode name x1 x2) = closest currNode (NewGraph nodes edges)
                putStr ("\nYour next closest Waypoint is" ++ name ++ "!")
                let result = (nextWaypoint (NewGraph nodes edges))
                return result


nextWaypoint :: (Graph [(Node [Char] Double Double)] [((Node [Char] Double Double), (Node [Char] Double Double), Double)]) -> IO String    
nextWaypoint (NewGraph nodes edges) =
    do
        let nodeNames = getNameofNodes nodes
        line <- ask ("Where would you like to continue your journey:\n" ++ nodeNames ++ "\n[Type stop at anytime to quit]\n> ")
        if (line=="stop") then 
            return "Thank you for travelling with Waypoint!"
            else do
                let currNode = getNodeofNames line nodes
                putStr ("\nYou have arrived at " ++ line ++ "!")
                let (NewNode name x1 x2) = closest currNode (NewGraph nodes edges)
                putStr ("\nYour next closest Waypoint is" ++ name ++ "!")
                return (nextWaypoint (NewGraph nodes edges))


-- Example Input of Nodes and Edges:
-- startup [3,1,2] [9,8,7]
startup :: n -> e -> (Graph n e)
startup n e = NewGraph n e
go :: IO String
go = waypoint