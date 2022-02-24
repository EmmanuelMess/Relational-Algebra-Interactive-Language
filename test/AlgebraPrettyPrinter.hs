module AlgebraPrettyPrinter where

import AST
import GeneralTypes
import RowPredicateType

import Data.List (intercalate)
  
printCommand :: Command -> String
printCommand Skip      = ""
printCommand Exit      = "exit"
printCommand (Print e) = "print (" ++ (printExpression e) ++ ")"
printCommand (Let name e) = name ++ " = " ++ (printExpression e)

printExpression :: Exp Relation -> String
printExpression (Var name) = name
printExpression (Union e1 e2) = printBinaryOperation "+" e1 e2
printExpression (CartesianProduct e1 e2) = printBinaryOperation "*" e1 e2
printExpression (Difference e1 e2) = printBinaryOperation "-" e1 e2

printExpression (Project attributes e) =
  "project(" ++ (intercalate ", " attributes) ++ ")(" ++ (printExpression e) ++ ")"
printExpression (Select p e) =
  "select(" ++ (printRowPredicate p) ++ ")(" ++ (printExpression e) ++ ")"
printExpression (Rename name e) =
  "rename(" ++ name ++ ")(" ++ (printExpression e) ++ ")"
  
printExpression (Intersection e1 e2) = printBinaryOperation "&" e1 e2
printExpression (NaturalProductSimple e1 e2) = printBinaryOperation "|*|" e1 e2

printExpression (NaturalProduct e1 e2 e3) = 
  printBinaryOperation ("|" ++ (printRowPredicate e2) ++ "|") e1 e3
  
printExpression (Division e1 e2) = printBinaryOperation "/" e1 e2

printBinaryOperation :: String -> Exp Relation -> Exp Relation -> String
printBinaryOperation operation e1 e2 = "(" ++ (printExpression e1) ++ operation ++ (printExpression e2) ++ ")"

printRowPredicate :: Exp RowPredicate -> String
printRowPredicate (Equal attribute value) =
  (printNamedAttribute attribute) ++ "=" ++ (printValue value)
printRowPredicate (NotEqual attribute value) =
  (printNamedAttribute attribute) ++ "<>" ++ (printValue value)
printRowPredicate (LessThan attribute value) =
  (printNamedAttribute attribute) ++ "<" ++ (printValue value)
printRowPredicate (LessEqualThan attribute value) =
  (printNamedAttribute attribute) ++ "<=" ++ (printValue value)
printRowPredicate (GreaterThan attribute value) =
  (printNamedAttribute attribute) ++ ">" ++ (printValue value)
printRowPredicate (GreaterEqualThan attribute value) =
  (printNamedAttribute attribute) ++ ">=" ++ (printValue value)
  
printRowPredicate (EqualAttributes attribute1 attribute2) =
  (printNamedAttribute attribute1) ++ "=" ++ (printNamedAttribute attribute2)
printRowPredicate (NotEqualAttributes attribute1 attribute2) =
  (printNamedAttribute attribute1) ++ "<>" ++ (printNamedAttribute attribute2)
printRowPredicate (LessThanAttributes attribute1 attribute2) =
  (printNamedAttribute attribute1) ++ "<" ++ (printNamedAttribute attribute2)
printRowPredicate (LessEqualThanAttributes attribute1 attribute2) =
  (printNamedAttribute attribute1) ++ "<=" ++ (printNamedAttribute attribute2)
printRowPredicate (GreaterThanAttributes attribute1 attribute2) =
  (printNamedAttribute attribute1) ++ ">" ++ (printNamedAttribute attribute2)
printRowPredicate (GreaterEqualThanAttributes attribute1 attribute2) =
  (printNamedAttribute attribute1) ++ ">=" ++ (printNamedAttribute attribute2)
  
printRowPredicate (And predicate1 predicate2) =
  "((" ++ (printRowPredicate predicate1) ++ ")&&(" ++ (printRowPredicate predicate2) ++ "))"
printRowPredicate (Or predicate1 predicate2) =
  "((" ++ (printRowPredicate predicate1) ++ ")||(" ++ (printRowPredicate predicate2) ++ "))"
  
printNamedAttribute :: AttributeWithRelation -> String
printNamedAttribute (Nothing, attribute) = attribute
printNamedAttribute (Just relation, attribute) = relation ++ "." ++ attribute
 
printValue :: RowValue -> String
printValue (S v) = "\"" ++ v ++ "\""
printValue (I v) = show v
printValue (F v) = show v
printValue (N)   = "N"