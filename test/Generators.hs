module Generators where

import           AST
import           Evaluation
import           FileChecker
import           GeneralTypes
import           StateMonad
import           RowPredicateType
  
import           Data.Either
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Map            as Map
  
import Test.QuickCheck
import Test.QuickCheck.Gen
  
instance Arbitrary (HeaderElement) where
  arbitrary = oneof [arbitraryString, arbitraryInteger, arbitraryFloating]

arbitraryString :: Gen HeaderElement
arbitraryString =
  do
    name <- genNormalVariable
    return (HeaderString name)

arbitraryInteger :: Gen HeaderElement
arbitraryInteger =
  do
    name <- genNormalVariable
    return (HeaderInteger name)

arbitraryFloating :: Gen HeaderElement
arbitraryFloating =
  do
    name <- genNormalVariable
    return (HeaderFloating name)

instance Arbitrary (RowValue) where
  arbitrary = oneof [arbitraryRowValueString, arbitraryRowValueInteger, arbitraryRowValueDouble, return N]

arbitraryRowValueString :: Gen RowValue
arbitraryRowValueString =
  do
    value <- genNormalVariable
    return (S value)

arbitraryRowValueInteger :: Gen RowValue
arbitraryRowValueInteger =
  do
    value <- arbitrary :: Gen Integer
    return (I value)

arbitraryRowValueDouble :: Gen RowValue
arbitraryRowValueDouble =
  do
    value <- arbitrary :: Gen Double
    return (F value)

genRelation :: Gen Relation
genRelation = 
  let
    genRawRelation = 
      (do
        relationNameGenerated <- genNormalVariable
        headerElements <- arbitrary :: Gen [HeaderElement]
        set <- arbitrary :: Gen (Set DatabaseRow)
        return (Just relationNameGenerated, headerElements, set))
  in genRawRelation `suchThat` (isRight . (relationChecker Set.empty))


instance Arbitrary (Command) where
  arbitrary = 
    oneof -- no Skip because it breaks on printing
      [ return Exit
      , do { x <- arbitrary; return (Print x); }
      , do { x <- genNormalVariable; y <- arbitrary; return (Let x y); }
      ]
    

instance Arbitrary (Exp Relation) where
  arbitrary =
    frequency 
      [ (20, do { x <- genNormalVariable; return (Var x); } )
      , (1, do { x <- arbitrary; y <- arbitrary; return (Union x y); } )
      , (1, do { x <- arbitrary; y <- arbitrary; return (CartesianProduct x y); } )
      , (1, do { x <- arbitrary; y <- arbitrary; return (Difference x y); } )
      
      , (1, do { n <- listOf1 genNormalVariable; y <- arbitrary; return (Project n y); } )
      , (1, do { p <- arbitrary; y <- arbitrary; return (Select p y); } )
      , (1, do { x <- genNormalVariable; y <- arbitrary; return (Rename x y); } )
      
      , (1, do { x <- arbitrary; y <- arbitrary; return (Intersection x y); } )
      , (1, do { x <- arbitrary; y <- arbitrary; return (NaturalProductSimple x y); } )
      , (1, do { x <- arbitrary; p <- arbitrary; y <- arbitrary; return (NaturalProduct x p y); } )
      
      , (1, do { x <- arbitrary; y <- arbitrary; return (Division x y); } )
      ]
      
instance Arbitrary (Exp RowPredicate) where
  arbitrary =
    frequency 
      [ (3, do { x <- genAttributeWithRelation; y <- arbitrary; return (Equal x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- arbitrary; return (NotEqual x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- arbitrary; return (LessThan x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- arbitrary; return (LessEqualThan x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- arbitrary; return (GreaterThan x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- arbitrary; return (GreaterEqualThan x y); } )
      
      , (3, do { x <- genAttributeWithRelation; y <- genAttributeWithRelation; return (EqualAttributes x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- genAttributeWithRelation; return (NotEqualAttributes x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- genAttributeWithRelation; return (LessThanAttributes x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- genAttributeWithRelation; return (LessEqualThanAttributes x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- genAttributeWithRelation; return (GreaterThanAttributes x y); } )
      , (3, do { x <- genAttributeWithRelation; y <- genAttributeWithRelation; return (GreaterEqualThanAttributes x y); } )
      
      , (1, do { x <- arbitrary; y <- arbitrary; return (And x y); } )
      , (1, do { x <- arbitrary; y <- arbitrary; return (Or x y); } )
      ]

genAttributeWithRelation :: Gen AttributeWithRelation
genAttributeWithRelation = 
  do
    x <- oneof [do { z <- genNormalVariable; return (Just z); }, return Nothing]
    y <- genNormalVariable
    return (x, y)

genNormalVariable :: Gen String
genNormalVariable = listOf1 (elements ['a'..'z'])

genEnv :: [String] -> Bool -> Gen Env
genEnv []            _           = return Map.empty
genEnv (relation:rs) sameHeaders =
  do
    (_, header, set) <- genRelation
    rest <- genEnv rs sameHeaders
    let newHeader = if Map.null rest || not sameHeaders then header else ((\(_, h, _) -> h) . snd) (Map.findMax rest)
    return (Map.insert relation (Just relation, newHeader, set) rest)