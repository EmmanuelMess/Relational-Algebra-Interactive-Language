module Errors where

import GeneralTypes
  
import Data.List
  
data RuntimeError
-- Operation errors
  = UndefinedVariable Variable
  | DifferentAttributes [AttributeWithRelation]
  | SameAttributes
  | MissingAttributes [AttributeName]
  | PredicateMissingAttributes AttributeName
  | AmbiguousAttribute AttributeName
  | PredicateMissingRelation RelationName
  | NoCommonAttributes
  | DivisionMissingAttributes [AttributeName]
-- Check file errors
  | NoNameOnCreation
  | EmptyHeaderName
  | NoAttributesInHeader
  | DuplicateRelation String
  | TooManyValuesInLine
  | MissingHeaderOrDuplicate AttributeName
  | TypeError RowValue HeaderElement
-- Parse errors
  | ParseError String

instance Show RuntimeError where
  show (UndefinedVariable name)  =
    "Error: " ++ name ++ " not declared"
  show (DifferentAttributes attrs) =
    "Error: Relations do not share all attributes, or attributes differ in type: " ++ (printList (map printAttribute attrs))
  show (SameAttributes) =
    "Error: Relations have attributes that are identical: [Ran out of time to implement the list of duplicates]"
  show (MissingAttributes attrs) =
    "Error: Relations do not have all projected attributes: " ++ (show attrs)
  show (PredicateMissingAttributes attr) =
    "Error: Undefined attribute in predicate: " ++ (show attr)
  show (AmbiguousAttribute attr) =
      "Error: Attribute in predicate is ambiguous: " ++ (show attr)
  show (PredicateMissingRelation name) =
    "Error: Undefined relation in predicate: " ++ (show name)
  show NoCommonAttributes =
    "Error: No common attributes, use cartesian product"
  show (DivisionMissingAttributes attrs) =
    "Error: Dividend does not have all attributes of divisor, or attributes differ in type: " ++ (show attrs)
  show (NoNameOnCreation) =
    "Error: Relation must have a name on creation"  
  show (EmptyHeaderName) =
    "Error: Relation with empty header names" 
  show (NoAttributesInHeader) =
    "Error: No attributes in header"
  show (DuplicateRelation name) =
    "Error: Duplicate relation " ++ name
  show (TooManyValuesInLine) = 
    "Error: in relation file line contains too many values"
  show (MissingHeaderOrDuplicate attribute) =
    "Error: Missing header for attribute or too many attributes with this name: " ++ attribute
  show (TypeError element header) = 
    "Error: Type error for " ++ (show element) ++ " should be of type " ++ (showAttributeType header)
  show (ParseError errorString) =
    errorString
  
printList :: [String] -> String
printList = intercalate ", "