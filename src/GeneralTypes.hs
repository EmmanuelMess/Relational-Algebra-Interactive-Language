module GeneralTypes where

import           Data.Maybe
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set

type Relation = (RelationName, [HeaderElement], Set DatabaseRow)

relationName :: Relation -> RelationName
relationName (name, _, _) = name

type Variable = String

type RelationName = Maybe Variable

type DatabaseRow = Map AttributeName RowValue
type NamedDatabaseRow = Map AttributeWithRelation RowValue

type AttributeWithRelation = (RelationName, AttributeName)

printAttribute :: AttributeWithRelation -> String
printAttribute (Nothing, _) = undefined
printAttribute (relationName', attributeName) = (fromJust relationName') ++ "." ++ attributeName

databaseRowToNamedDatabaseRow :: RelationName -> DatabaseRow -> NamedDatabaseRow
databaseRowToNamedDatabaseRow relationName' databaseRow =
  Map.mapKeys (\k -> (relationName', k)) databaseRow

databaseRowsToNamedDatabaseRows :: RelationName -> Set DatabaseRow -> Set NamedDatabaseRow
databaseRowsToNamedDatabaseRows relationName' databaseRows =
  Set.map (databaseRowToNamedDatabaseRow relationName') databaseRows


namedDatabaseRowToDatabaseRow :: NamedDatabaseRow -> DatabaseRow
namedDatabaseRowToDatabaseRow databaseRow =
  Map.mapKeys (\(_, k) -> k) databaseRow

nameDatabaseRowsToDatabaseRows :: Set NamedDatabaseRow -> Set DatabaseRow
nameDatabaseRowsToDatabaseRows databaseRows =
  Set.map namedDatabaseRowToDatabaseRow databaseRows

data RowValue
  = S String
  | I Integer
  | F Double
  | N
  deriving (Show, Eq, Ord)

data HeaderElement
  = HeaderString AttributeName
  | HeaderInteger AttributeName
  | HeaderFloating AttributeName
  deriving (Show, Eq)

type AttributeName = String

headerAttribute :: HeaderElement -> AttributeName
headerAttribute (HeaderString x) = x
headerAttribute (HeaderInteger x) = x
headerAttribute (HeaderFloating x) = x

showAttributeType :: HeaderElement -> String
showAttributeType (HeaderString _) = "String"
showAttributeType (HeaderInteger _) = "Integer"
showAttributeType (HeaderFloating _) = "Floating"

replaceAttribute :: HeaderElement -> AttributeName -> HeaderElement
replaceAttribute (HeaderString _)   x = HeaderString x
replaceAttribute (HeaderInteger _)  x = HeaderInteger x
replaceAttribute (HeaderFloating _) x = HeaderFloating x

emptyRelation :: Relation
emptyRelation = (Nothing, [], Set.empty)