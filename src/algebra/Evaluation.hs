module Evaluation where

import           AST
import           GeneralTypes
import           StateMonad
import           Errors
import           PrettyPrinter
import           RowPredicateType

import           Control.Monad.Except

import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Map             as Map
import           Data.List
import           Data.Either
import           Data.Tuple           (swap)

eval :: Env -> Command -> (Env, Either RuntimeError String)
eval env p = swap (runState (runExceptT (stepCommStar p)) env)

stepCommStar :: Command -> ExceptT RuntimeError State String
stepCommStar Skip              = return ""
stepCommStar (Print x) =
  do
    a <- evalExp x
    return (renderRelation a)
stepCommStar c                 = stepComm c >>= \c' -> stepCommStar c'

stepComm :: Command -> ExceptT RuntimeError State Command
stepComm (Let v e)     =
  do
    x <- evalExp e
    update v x
    return Skip
stepComm Skip         = undefined -- Already managed in stepCommStar
stepComm Exit         = undefined -- Already managed in stepCommStar
stepComm (Print _)    = undefined -- Already managed in stepCommStar

-- Evalua una expresion
evalExp :: Exp a -> ExceptT RuntimeError State a
evalExp (Var c1)                 = lookfor c1
evalExp (Union e1 e2)            =
  do
    (nameA, attributesA, rowsA) <- evalExp e1
    (nameB, attributesB, rowsB) <- evalExp e2
    
    let resultRows = Set.union rowsA rowsB 
    let namedFieldsA = map ((\x -> (nameA, x)) . headerAttribute) (attributesA \\ attributesB)
    let namedFieldsB = map ((\x -> (nameB, x)) . headerAttribute) (attributesB \\ attributesA)
    let differenceAttributes = namedFieldsA ++ namedFieldsB
    
    (if (not . null) differenceAttributes
      then throwError (DifferentAttributes differenceAttributes)
      else return (Nothing, attributesA, resultRows))
evalExp (CartesianProduct e1 e2) =
  do
    (_, attributesA, rowsA) <- evalExp e1
    (_, attributesB, rowsB) <- evalExp e2

    let attributesResult = attributesA ++ attributesB
    let rowsResult = Set.unions (Set.map (\rowA -> Set.map (\rowB -> Map.union rowA rowB) rowsB) rowsA)
    let commonAttributes = attributesResult \\ (nub attributesResult)

    if (not.null) commonAttributes
    then throwError SameAttributes
    else return (Nothing, attributesResult, rowsResult)
evalExp (Difference e1 e2) =
  do
    (nameARaw, headersA, rowsA) <- evalExp e1
    (nameBRaw, headersB, rowsB) <- evalExp e2
    let attributesAName = map headerAttribute headersA
    let attributesBName = map headerAttribute headersB
    let nameA = if nameARaw == Nothing then (Just "$FirstOperand") else nameARaw -- TODO make idiomatic (fmap?)
    let nameB = if nameBRaw == Nothing then (Just "$SecondOperand") else nameBRaw

    let difference = Set.difference rowsA rowsB

    (if attributesAName /= attributesBName
      then throwError (DifferentAttributes ((map (nameA,) attributesAName) ++ (map (nameB,) attributesBName)))
      else return (Nothing, headersA, difference))

evalExp (Project v e) =
  do
    (_, attributesA, rowsA) <- evalExp e
    let attributesResult = filter ((\x -> any (x ==) v) . headerAttribute) attributesA
    let attrsA = map headerAttribute attributesA
    let missing = filter (\x -> all (x /=) attrsA) v
    let rowsResult = Set.map (Map.filterWithKey (\k _ -> any (k ==) v)) rowsA

    (if (not . null) missing
      then throwError (MissingAttributes missing)
      else return (Nothing, attributesResult, rowsResult))
evalExp (Select b e) =
  do
    condition <- evalExp b
    (nameA, attributesA, rowsA) <- evalExp e
    let namedRows = databaseRowsToNamedDatabaseRows nameA rowsA
    let relationNames = Set.singleton nameA
    let ambiguousRelations = Set.empty -- All attributes are A's attributes

    filtered <- (liftEither . runExcept) (monadFilter (condition relationNames ambiguousRelations) namedRows)
    let rowsResult = nameDatabaseRowsToDatabaseRows filtered

    return (Nothing, attributesA, rowsResult)
evalExp (Rename newName e) =
  do
    (_, attributesA, rowsA) <- evalExp e
    return (Just newName, attributesA, rowsA)

evalExp (Intersection e1 e2) =
  do
    (_, attributesA, rowsA) <- evalExp e1
    (_, attributesB, rowsB) <- evalExp e2

    let attributesResult = filter (\x -> any (x ==) attributesA) attributesB
    let rowsResult = Set.intersection rowsA rowsB

    return (Nothing, attributesResult, rowsResult)
evalExp (NaturalProductSimple e1 e2) =
  do
    (_, attributesA, rowsA) <- evalExp e1
    (_, attributesB, rowsB) <- evalExp e2

    let attributesResult = nub (attributesA ++ attributesB)
    let rowsResult = Set.unions (Set.map (\rowA -> Set.map (\rowB -> Map.union rowA rowB) rowsB) rowsA)

    let selectiveAttributes = intersect attributesA attributesB

    let selectiveAttributeNames = map headerAttribute selectiveAttributes
    let filterSelectiveCols = Map.filterWithKey (\k _ -> elem k selectiveAttributeNames)
    let filteredRows = Set.filter ((\x -> Set.member x rowsB) . filterSelectiveCols) rowsResult

    let selectedAttributeNames = map headerAttribute attributesResult
    let resultRows = Set.map (Map.filterWithKey (\k _ -> elem k selectedAttributeNames)) filteredRows

    (if null selectiveAttributes
      then throwError NoCommonAttributes
      else return (Nothing, attributesResult, resultRows))
evalExp (NaturalProduct e1 b e2) =
  do
    (nameA, attributesA, rowsA) <- evalExp e1
    p <- evalExp b
    (nameB, attributesB, rowsB) <- evalExp e2

    let attributesResult = nub (attributesA ++ attributesB)
    let namedARows = databaseRowsToNamedDatabaseRows nameA rowsA
    let namedBRows = databaseRowsToNamedDatabaseRows nameB rowsB
    let namedRowsUnited = Set.unions (Set.map (\rowA -> Set.map (\rowB -> Map.union rowA rowB) namedBRows) namedARows)
    let relationNames = Set.fromList [nameA, nameB]
    let ambiguousRelations = Set.fromList (intersect (map headerAttribute attributesA) (map headerAttribute attributesB))

    filteredRows <- (liftEither . runExcept) (monadFilter (p relationNames ambiguousRelations) namedRowsUnited)
    let resultRows = nameDatabaseRowsToDatabaseRows filteredRows

    return (Nothing, attributesResult, resultRows)
    
evalExp (Division e1 e2) =
  do
    (_, attributesA, rowsA) <- evalExp e1
    (_, attributesB, rowsB) <- evalExp e2

    let selectiveAttributes = intersect attributesA attributesB
    let selectedAttributes = attributesA \\ attributesB

    let selectiveAttributeNames = map headerAttribute selectiveAttributes
    let filterSelectiveCols = Map.filterWithKey (\k _ -> elem k selectiveAttributeNames)
    let filteredRows = Set.filter ((\x -> Set.member x rowsB) . filterSelectiveCols) rowsA

    let selectedAttributeNames = map headerAttribute selectedAttributes
    let resultRows = Set.map (Map.filterWithKey (\k _ -> elem k selectedAttributeNames)) filteredRows

    (if selectiveAttributes /= attributesB
      then throwError (DivisionMissingAttributes (map headerAttribute (attributesB \\ selectiveAttributes)))
      else return (Nothing, selectedAttributes, resultRows))

evalExp (Equal x y)                      = checkPreconditionsAndApply x y (==)
evalExp (NotEqual x y)                   = checkPreconditionsAndApply x y (/=)
evalExp (LessThan x y)                   = checkPreconditionsAndApply x y (<)
evalExp (LessEqualThan x y)              = checkPreconditionsAndApply x y (<=)
evalExp (GreaterThan x y)                = checkPreconditionsAndApply x y (>)
evalExp (GreaterEqualThan x y)           = checkPreconditionsAndApply x y (>=)

evalExp (EqualAttributes x y)            = checkPreconditionsAndApply2 x y (==)
evalExp (NotEqualAttributes x y)         = checkPreconditionsAndApply2 x y (/=)
evalExp (LessThanAttributes x y)         = checkPreconditionsAndApply2 x y (<)
evalExp (LessEqualThanAttributes x y)    = checkPreconditionsAndApply2 x y (<=)
evalExp (GreaterThanAttributes x y)      = checkPreconditionsAndApply2 x y (>)
evalExp (GreaterEqualThanAttributes x y) = checkPreconditionsAndApply2 x y (>=)

evalExp (And c1 c2) =
  do 
    p1 <- evalExp c1
    p2 <- evalExp c2

    return (\names ambiguousAttributes row ->
             do {
                  r1 <- p1 names ambiguousAttributes row;
                  r2 <- p2 names ambiguousAttributes row;
                  return (r1 && r2)
                })
evalExp (Or c1 c2) =
  do 
    p1 <- evalExp c1
    p2 <- evalExp c2
    return (\names ambiguousAttributes row ->
             do {
                  r1 <- p1 names ambiguousAttributes row;
                  r2 <- p2 names ambiguousAttributes row;
                  return (r1 || r2)
                })

monadFilter :: SimpleRowPredicate -> Set NamedDatabaseRow -> Except RuntimeError (Set NamedDatabaseRow)
monadFilter condition rows =
  let
    filtered = Set.filter (isLeft . runExcept . condition) rows
  in if (not . Set.null) filtered
     then throwError ((unsafeFromLeft . runExcept . condition . Set.findMin) filtered)
     else return (Set.filter (unsafeFromRight . runExcept . condition) rows)

simmetricSustract :: Eq a => [a] -> [a] -> [a]
simmetricSustract x y = (x \\ y) ++ (y \\ x)

checkPreconditionsAndApply :: AttributeWithRelation -> RowValue -> (RowValue -> RowValue -> Bool) -> ExceptT RuntimeError State RowPredicate
checkPreconditionsAndApply x@(relationNameA, _) str f =
  return
    (\names ambiguousAttributes row ->
      do
        _ <- (if relationNameA /= Nothing && not (Set.member relationNameA names)
              then throwError (PredicateMissingRelation relationNameA)
              else return True)
        _ <- (if not (Set.member (snd x) (Set.map (\(_, attr) -> attr) (Map.keysSet row)))
              then throwError (PredicateMissingAttributes (snd x))
              else return True)
        _ <- (if (fst x) == Nothing && Set.member (snd x) ambiguousAttributes
              then throwError (AmbiguousAttribute (snd x))
              else return True)

        let row' = Map.union row (Map.mapKeys (\(_, attribute) -> (Nothing, attribute)) row)
        return (f (row' Map.! x) str))

checkPreconditionsAndApply2 :: AttributeWithRelation -> AttributeWithRelation -> (RowValue -> RowValue -> Bool) -> ExceptT RuntimeError State RowPredicate
checkPreconditionsAndApply2 x@(relationNameA, _) y@(relationNameB, _) f =
  return
    (\names ambiguousAttributes row ->
      do
        _ <- (if relationNameA /= Nothing && not (Set.member relationNameA names)
              then throwError (PredicateMissingRelation relationNameA)
              else return True)
        _ <- (if relationNameB /= Nothing && not (Set.member relationNameB names)
             then throwError (PredicateMissingRelation relationNameB)
             else return True)
        _ <- (if not (Set.member (snd x) (Set.map (\(_, attr) -> attr) (Map.keysSet row)))
              then throwError (PredicateMissingAttributes (snd x))
              else return True)
        _ <- (if not (Set.member (snd y) (Set.map (\(_, attr) -> attr) (Map.keysSet row)))
              then throwError (PredicateMissingAttributes (snd y))
              else return True)
        _ <- (if (fst x) == Nothing && Set.member (snd x) ambiguousAttributes
              then throwError (AmbiguousAttribute (snd x))
              else return True)
        _ <- (if (fst y) == Nothing && Set.member (snd y) ambiguousAttributes
              then throwError (AmbiguousAttribute (snd y))
              else return True)

        let row' = Map.union row (Map.mapKeys (\(_, attribute) -> (Nothing, attribute)) row)
        return (f (row' Map.! x) (row' Map.! y)))

unsafeFromLeft :: Either a b -> a
unsafeFromLeft (Right _) = undefined
unsafeFromLeft (Left x)  = x

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight (Left _)  = undefined