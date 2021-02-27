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



-- Example Input of Nodes and Edges:
-- startup [3,1,2] [9,8,7]
startup :: n -> e -> (Graph n e)
startup n e = NewGraph n e