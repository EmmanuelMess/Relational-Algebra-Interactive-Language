module PrettyPrinter where

import           GeneralTypes

import qualified Data.Map                as Map

import           Data.Set                (Set)
import qualified Data.Set                as Set

import           Data.List

import           Text.PrettyPrint.Boxes

import           Prelude                 hiding ( (<>), (<$>) )

tabW :: Int
tabW = 2

renderRelation :: Relation -> String
renderRelation = render . pRelation

pRelation :: Relation -> Box
pRelation (name, headers, relationRows) = 
  let 
    box =  (pName name // pHeaders headers // pRows headers relationRows) 
    vsize = 1 + 1 + (Set.size relationRows)
  in align center1 center1 vsize 80 box


pName :: RelationName -> Box
pName (Just name) = text name
pName (Nothing)   = text ""

pHeaders :: [HeaderElement] -> Box
pHeaders headers = hsep 1 top (map (pHeader) headers)

pHeader :: HeaderElement -> Box
pHeader (HeaderString (x))   = text (x ++ "(S)")
pHeader (HeaderInteger (x))  = text (x ++ "(I)")
pHeader (HeaderFloating (x)) = text (x ++ "(F)")

pRows :: [HeaderElement] -> Set DatabaseRow -> Box
pRows headers relationRows = vcat top (map (pRow headers) (Set.toList relationRows))

pRow :: [HeaderElement] -> DatabaseRow -> Box
pRow headers row = hsep 1 top (map (pRowValue . (\x -> row Map.! x) . headerAttribute) headers)

pRowValue :: RowValue -> Box
pRowValue (S x) = (text . show) x
pRowValue (I x) = (text . show) x
pRowValue (F x) = (text . show) x
pRowValue N     = text "N"
