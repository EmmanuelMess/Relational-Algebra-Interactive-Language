module Parser where

import           AST
import           GeneralTypes
import           RowPredicateType

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )

---------------------------------------------------------

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = 
  do
    whiteSpace lis
    t <- p
    eof
    return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { opLetter        = char '='
    , reservedNames   = ["exit", "print"]
    , reservedOpNames = [ ".", "+", "-", "*", "&", "/", "|*|", "|", "=", "<", ">", "<>", "<=", ">=", "&&", "||", "(", ")", "project", "select", "rename"]
    }
  )
  
commandParser :: Parser Command
commandParser =
  do
    reserved lis "exit"
    return Exit
  <|> do
    reserved lis "print"
    valor <- relationParser
    return (Print valor)
  <|> do
    nombre <- identifier lis
    reservedOp lis "="
    valor <- relationParser
    return (Let nombre valor)

relationParser :: Parser (Exp Relation)
relationParser = naturalOperationParser `chainl1` binaryRelationParser
    
naturalOperationParser :: Parser (Exp Relation)
naturalOperationParser = 
  do
    operationsParser
  <|> do
    operand <- identifier lis
    return (Var operand) 
  <|> do
    parens lis relationParser

binaryRelationParser :: Parser (Exp Relation -> Exp Relation -> Exp Relation)
binaryRelationParser =
  do
    oneBinaryRelationParser "+" Union
  <|> do
    oneBinaryRelationParser "*" CartesianProduct
  <|> do
    oneBinaryRelationParser "-" Difference
  <|> do
    oneBinaryRelationParser "&" Intersection
  <|> do
    oneBinaryRelationParser "/" Division
  <|> do
    oneBinaryRelationParser "|*|" NaturalProductSimple
  <|> do
    reservedOp lis "|"
    condition <- booleanParser
    reservedOp lis "|"
    return (\x y -> NaturalProduct x condition y)

oneBinaryRelationParser :: String -> (Exp Relation -> Exp Relation -> Exp Relation) -> Parser (Exp Relation -> Exp Relation -> Exp Relation)
oneBinaryRelationParser operation f =
  do
    reservedOp lis operation
    return f

operationsParser :: Parser (Exp Relation)
operationsParser =
  do
    reservedOp lis "project"
    attributeList <- parens lis (commaSep1 lis (identifier lis))
    r <- parens lis relationParser
    return (Project attributeList r)
  <|> do
    reservedOp lis "select"
    booleanExpression <- parens lis booleanParser
    r <- parens lis relationParser
    return (Select booleanExpression r)
  <|> do
    reservedOp lis "rename"
    originalName <- parens lis (identifier lis)
    r <- parens lis relationParser
    return (Rename originalName r)

booleanParser :: Parser (Exp RowPredicate)
booleanParser = simpleBooleanParser `chainl1` binaryBooleanParser
  
simpleBooleanParser :: Parser (Exp RowPredicate)
simpleBooleanParser =
  do
    try (oneSimpleBooleanParser "=" Equal EqualAttributes)
  <|> do
    try (oneSimpleBooleanParser "<>" NotEqual NotEqualAttributes)

  <|> do
     try (oneSimpleBooleanParser "<" LessThan LessThanAttributes)
  <|> do
    try (oneSimpleBooleanParser "<=" LessEqualThan LessEqualThanAttributes)

  <|> do
    try (oneSimpleBooleanParser ">" GreaterThan GreaterThanAttributes)
  <|> do
    try (oneSimpleBooleanParser ">=" GreaterEqualThan GreaterEqualThanAttributes)
    
  <|> do
      parens lis booleanParser

oneSimpleBooleanParser :: String -> (AttributeWithRelation -> RowValue -> Exp RowPredicate) -> (AttributeWithRelation -> AttributeWithRelation -> Exp RowPredicate) -> Parser (Exp RowPredicate)
oneSimpleBooleanParser operation f g =
  do
    nombre <- attributeParser
    reservedOp lis operation
    (do
      value <- try parseRowAttributeValue
      return (f nombre value)
     <|> do
       value <- attributeParser
       return (g nombre value))

binaryBooleanParser :: Parser (Exp RowPredicate -> Exp RowPredicate -> Exp RowPredicate)
binaryBooleanParser = 
  do
    reservedOp lis "&&"
    return And
  <|> do
    reservedOp lis "||"
    return Or

attributeParser :: Parser (RelationName, AttributeName)
attributeParser =
  do
    try (do  -- check for a dot
      r <- identifier lis
      reservedOp lis "."
      attr <- identifier lis
      return (Just r, attr))
  <|> do
    attr <- identifier lis
    return (Nothing, attr)
  <|> do 
    parens lis attributeParser

parseRowAttributeValue :: Parser RowValue
parseRowAttributeValue = 
  do
    reservedOp lis "\""
    value <- many alphaNum
    reservedOp lis "\""
    return (S value)
  <|> do
    s <- char '-' <|> char '+' <|> return '+' <?> "" --to reduce confusion
    value <- naturalOrFloat lis
    return (case value of
      Left x -> I (if s == '+' then x else -x) -- these two lines have different types
      Right x -> F (if s == '+' then x else -x))
  <|> do
    reservedOp lis "N"
    return N
  <|> do 
    parens lis parseRowAttributeValue