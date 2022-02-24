module FilePrettyPrinter where

import           GeneralTypes

import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Data.List           (intercalate)
  
printRelationToFile :: Relation -> String
printRelationToFile (Nothing, _, _) = undefined
printRelationToFile (Just name, headers, rows) =
  name ++ "\n" ++ (printHeadersToFile headers) ++ "\n" ++ (printRowsToFile headers rows)
  
printHeadersToFile :: [HeaderElement] -> String
printHeadersToFile headers =
  intercalate " " (map printHeaderToFile headers)
  
printHeaderToFile :: HeaderElement -> String
printHeaderToFile (HeaderString name)   = name ++ "(S)"
printHeaderToFile (HeaderInteger name)  = name ++ "(I)"
printHeaderToFile (HeaderFloating name) = name ++ "(F)"

printRowsToFile :: [HeaderElement] -> Set DatabaseRow -> String
printRowsToFile headers rows =
  intercalate "\n" (map (printRowToFile headers) (Set.toList rows))
  
printRowToFile :: [HeaderElement] -> DatabaseRow -> String
printRowToFile headers row =
  let 
    names = map headerAttribute headers
  in intercalate " " (map (show . (row Map.!)) names)