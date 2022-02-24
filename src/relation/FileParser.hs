module FileParser where

import           GeneralTypes

import           Text.ParserCombinators.Parsec -- TODO use Text.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )

import qualified Data.Map                       as Map
import           Data.Set                       ( Set )
import qualified Data.Set                       as Set
import           Data.List.Split

-- Analizador de Tokens
headerLis :: TokenParser u
headerLis = makeTokenParser
  (emptyDef
    { reservedNames   = ["(I)", "(F)", "(S)"]
    , reservedOpNames = ["\""]
    }
  )
  
contentLis :: TokenParser u
contentLis = makeTokenParser
  (emptyDef
    { reservedNames   = ["N"]
    , reservedOpNames = ["\""]
    }
  )
  
parseRelation :: Parser Relation
parseRelation =
  do
    name <- identifier headerLis
    header <- parseHeader
    rows <- parseRows header
    return (Just name, header, rows)

parseHeader :: Parser [HeaderElement]
parseHeader = 
  do
    many (parseHeaderAttribute)
  <|> do 
    return []

parseHeaderAttribute :: Parser HeaderElement
parseHeaderAttribute = 
  do
    name <- identifier headerLis 
    parens headerLis (
      do
        reservedOp headerLis "I"
        return (HeaderInteger name)
      <|> do
        reservedOp headerLis "F"
        return (HeaderFloating name)
      <|> do
        reservedOp headerLis "S"
        return (HeaderString name))
 
parseRows :: [HeaderElement] -> Parser (Set DatabaseRow)
parseRows headerFields =
  do
    rawRows <- many parseRowAttributeValue
    let attributes = map headerAttribute headerFields
    let segmentedRawRows = chunksOf (length headerFields) rawRows
    let parsedRows = map (Map.fromList . zip attributes) segmentedRawRows
    let setOfRows = Set.fromList parsedRows
    return setOfRows

parseRowAttributeValue :: Parser RowValue
parseRowAttributeValue = 
  do
    reservedOp contentLis "\""
    value <- many alphaNum
    reservedOp contentLis "\""
    return (S value)
  <|> do
    s <- char '-' <|> char '+' <|> return '+' <?> "" --to reduce confusion
    value <- naturalOrFloat contentLis
    return (case value of
      Left x -> I (if s == '+' then x else -x) -- these two lines have different types
      Right x -> F (if s == '+' then x else -x))
  <|> do
    reservedOp contentLis "N"
    return N