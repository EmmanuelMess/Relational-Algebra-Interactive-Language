-- This module prevents an import cycle
module RowPredicateType where

import GeneralTypes
import Errors
  
import           Data.Set            (Set)

import Control.Monad.Except

type RowPredicate = Set RelationName -> Set AttributeName -> NamedDatabaseRow -> Except RuntimeError Bool
type SimpleRowPredicate = NamedDatabaseRow -> Except RuntimeError Bool