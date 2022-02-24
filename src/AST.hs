module AST where

import           GeneralTypes
import           RowPredicateType

data Exp a where
  -- Relation
  Var :: Variable -> Exp Relation
  Union :: Exp Relation -> Exp Relation -> Exp Relation
  CartesianProduct :: Exp Relation -> Exp Relation -> Exp Relation
  Difference :: Exp Relation -> Exp Relation -> Exp Relation
  
  Project :: [AttributeName] -> Exp Relation -> Exp Relation
  Select :: Exp RowPredicate -> Exp Relation -> Exp Relation
  Rename :: Variable -> Exp Relation -> Exp Relation
  
  Intersection :: Exp Relation -> Exp Relation -> Exp Relation
  NaturalProductSimple :: Exp Relation -> Exp Relation -> Exp Relation
  NaturalProduct :: Exp Relation -> Exp RowPredicate -> Exp Relation -> Exp Relation
  
  Division :: Exp Relation -> Exp Relation -> Exp Relation
  
  -- Boolean
  
  -- Against constant
  
  Equal :: AttributeWithRelation -> RowValue -> Exp RowPredicate
  NotEqual :: AttributeWithRelation -> RowValue -> Exp RowPredicate
  LessThan :: AttributeWithRelation -> RowValue -> Exp RowPredicate
  LessEqualThan :: AttributeWithRelation -> RowValue -> Exp RowPredicate
  GreaterThan :: AttributeWithRelation -> RowValue -> Exp RowPredicate
  GreaterEqualThan :: AttributeWithRelation -> RowValue -> Exp RowPredicate

  -- Against attribute
  EqualAttributes :: AttributeWithRelation -> AttributeWithRelation -> Exp RowPredicate
  NotEqualAttributes :: AttributeWithRelation -> AttributeWithRelation -> Exp RowPredicate
  LessThanAttributes :: AttributeWithRelation -> AttributeWithRelation -> Exp RowPredicate
  LessEqualThanAttributes :: AttributeWithRelation -> AttributeWithRelation -> Exp RowPredicate
  GreaterThanAttributes :: AttributeWithRelation -> AttributeWithRelation -> Exp RowPredicate
  GreaterEqualThanAttributes :: AttributeWithRelation -> AttributeWithRelation -> Exp RowPredicate
  
  And :: Exp RowPredicate -> Exp RowPredicate -> Exp RowPredicate
  Or :: Exp RowPredicate -> Exp RowPredicate -> Exp RowPredicate

deriving instance Show (Exp a)
deriving instance Eq (Exp a)

data Command
  = Skip
  | Exit
  | Print (Exp Relation)
  | Let Variable (Exp Relation)
  deriving (Show, Eq)