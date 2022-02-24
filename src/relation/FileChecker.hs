module FileChecker where

import           Errors
import           GeneralTypes

import qualified Data.Map            as Map
import           Data.Set (Set)
import qualified Data.Set as Set

relationChecker :: Set RelationName -> Relation -> Either RuntimeError ()
relationChecker _ (Nothing, _, _) = Left NoNameOnCreation
relationChecker _ (Just "", _, _) = Left NoNameOnCreation
relationChecker parsedRelationNames (name@(Just relationName'), fileFields, fileRows) =
  do
    (if Set.member name parsedRelationNames
     then Left (DuplicateRelation relationName')
     else Right ())
    relationHeadersChecker fileFields
    relationRowsChecker fileFields fileRows
    
relationHeadersChecker :: [HeaderElement] -> Either RuntimeError ()
relationHeadersChecker fields = 
  do 
    _ <- mapM_ ((\x -> if x == "" then Left EmptyHeaderName else Right ()) . headerAttribute) fields
    (if null fields then Left NoAttributesInHeader else Right ())
   
relationRowsChecker :: [HeaderElement] -> Set DatabaseRow -> Either RuntimeError ()
relationRowsChecker fields rows = mapM_ (relationRowChecker fields) rows

relationRowChecker :: [HeaderElement] -> DatabaseRow -> Either RuntimeError ()
relationRowChecker fields row =
  do
    relationRowLengthChecker (length fields) row
    let rowList = Map.toList row
    mapM_ (rowElementTypesCheck fields) rowList

relationRowLengthChecker :: Int -> DatabaseRow -> Either RuntimeError ()
relationRowLengthChecker x row | x == (length row) = Right ()
                               | otherwise         = Left TooManyValuesInLine

rowElementTypesCheck :: [HeaderElement] -> (AttributeName, RowValue) -> Either RuntimeError ()
rowElementTypesCheck fields (attribute, value) =
  let
    x = filter ((attribute ==) . headerAttribute) fields
  in if length x /= 1
     then Left (MissingHeaderOrDuplicate attribute)
     else rowElementTypeCheck (head x) value

rowElementTypeCheck :: HeaderElement -> RowValue -> Either RuntimeError ()
rowElementTypeCheck (HeaderString _)   (S _)    = Right ()
rowElementTypeCheck (HeaderInteger _)  (I _)    = Right ()
rowElementTypeCheck (HeaderFloating _) (F _)    = Right ()
rowElementTypeCheck _                  N        = Right () -- "N" is type ambiguous in file
rowElementTypeCheck header             element  = Left (TypeError element header)