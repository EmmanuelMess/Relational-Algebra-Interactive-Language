module Main where

import           AST
import           GeneralTypes
import           StateMonad
import           FilePrettyPrinter
import           FileParser
import           Evaluation
import           Generators
import           Parser
import           AlgebraPrettyPrinter
import           Errors (RuntimeError)

import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Data.Either

import           Test.QuickCheck

import           Text.ParserCombinators.Parsec


main :: IO ()
main =
  do
    -- check parsers
    quickCheck (forAll genRelation propFileParser)
    quickCheck propCommandParser

    -- check operations simple
    quickCheck (forAll (genEnv ["RelA", "RelB"] True) (simpleUnionChecks "RelA" "RelB"))

    -- check operations properties
    quickCheck (forAll genRelation propOperationDifferenceEmpty)
    quickCheck (forAll genNormalVariable (\x -> forAll genNormalVariable (\y -> forAll genNormalVariable (\z -> propParseBasic x y z))))


propFileParser :: Relation -> Bool
propFileParser relation =
 let
   fileContents = printRelationToFile relation
   parsedFile = parse (totParser parseRelation) "test file" fileContents
 in parsedFile == (Right relation)
 
propCommandParser :: Command -> Bool
propCommandParser command =
 let
   commandPrinted = printCommand command
   parsedCommand = simpleCommandParser commandPrinted
 in parsedCommand == command

simpleUnionChecks :: Variable -> Variable -> Env -> Bool
simpleUnionChecks a b e =
  let
    commands = ["X = " ++ a ++ " + " ++ b, "Y = X & " ++ a, "Z = X &" ++ b]
    parsedCommands = map simpleCommandParser commands
    commandsExecuted = executeInOrder e parsedCommands
  in
    isRight commandsExecuted
     && unnamedCompare ((unsafeRight commandsExecuted) Map.! "Y") ((unsafeRight commandsExecuted) Map.! a)
     && unnamedCompare ((unsafeRight commandsExecuted) Map.! "Z") ((unsafeRight commandsExecuted) Map.! b)

propOperationDifferenceEmpty :: Relation -> Bool
propOperationDifferenceEmpty relationA@(_, attribute, _) =
 let
   environment = [("X", relationA), ("Y", relationA)]
   command = Let "Z" (Difference (Var "X") (Var "Y"))
 in (fst (eval (initEnv environment) command)) Map.! "Z" == (Nothing, attribute, Set.empty)

propParseBasic :: Variable -> Variable -> Variable -> Bool
propParseBasic a b c =
  let
    input = a ++ " = " ++ b ++ " + " ++ c
    parsed = simpleCommandParser input
  in parsed == (Let a (Union (Var b) (Var c)))

simpleCommandParser :: String -> Command
simpleCommandParser c = unsafeRight (parse (totParser Parser.commandParser) "" c)

executeInOrder :: Env -> [Command] -> Either RuntimeError Env
executeInOrder e []           = Right e
executeInOrder e (command:cs) =
  do
    let (e', r) = eval e command
    _ <- r
    executeInOrder e' cs

unsafeRight :: (Either a b) -> b
unsafeRight (Left _) = undefined
unsafeRight (Right x) = x
  
unnamedCompare :: Relation -> Relation -> Bool
unnamedCompare (_, headersA, rowsA) (_, headersB, rowsB) = 
  headersA == headersB && rowsA == rowsB