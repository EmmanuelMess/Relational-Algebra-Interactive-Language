module Main where

import           System.Directory
import           System.IO
import           System.Console.Haskeline

import           Control.Monad.Except

import           Data.List
import           Data.Maybe
import           Data.Bifunctor
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           AST
import           Errors
import           Evaluation
import           FileChecker
import           FileParser
import           GeneralTypes
import           Parser
import           StateMonad

import           Text.ParserCombinators.Parsec

main :: IO ()
main = 
  let
    f relationsAndNames = case relationsAndNames of
            Right (relations, names) -> (do
                          outputStrLn ("Loaded relations: " ++ printList names)
                          loop (initEnv (map (\r@(Just n, _, _) -> (n, r)) relations)))
            Left e -> outputStrLn (show e)
  in do  
    relationsAndNames <- runExceptT loadRelationsFromFiles
    runInputT defaultSettings (f relationsAndNames)

loadRelationsFromFiles :: ExceptT RuntimeError IO ([Relation], [Variable])
loadRelationsFromFiles = 
  do
    relationFilenamesList <- liftIO allRelationFilenames
    relationFileContentList <- parseFromSources relationFilenamesList    
    return (relationFileContentList, map (fromJust . relationName) relationFileContentList)

loop :: Env -> InputT IO ()
loop env = 
  do
    outputStrLn "Relational Algebra Interactive Language (RAIL) by Facundo Emmanuel Messulam (2021-2022)"
    minput <- getInputLine "RAIL> "
    case minput of
      Nothing -> return ()
      Just "" -> loop env
      Just input -> do
        liftIO $ parseTest (totParser commandParser) input
        case parse (totParser commandParser) "" input of
          Left errorString -> do
            outputStrLn (show errorString)
            loop env
          Right Exit -> return ()
          Right x    -> do
            (case eval env x of
              (env', Left errorString) -> do
                outputStrLn (show errorString)
                loop env'
              (env', Right "") -> loop env'
              (env', Right s)  -> do
                outputStrLn s
                loop env')

allRelationFilenames :: IO [String]
allRelationFilenames = 
  do
    files <- getDirectoryContents relationsDirectory
    return (map (relationsDirectory ++) (filter (isSuffixOf ".r") files))

parseFromSources :: [String] -> ExceptT RuntimeError IO [Relation]
parseFromSources filesContent =
  let
    fun usedNames content =
      do
        relation <- relationFileContents usedNames content
        return (Set.insert (relationName relation) usedNames, relation)
  in do
      (_, relations) <- mapAccumLM fun Set.empty filesContent
      return relations

mapAccumLM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM _ a [] = return (a, [])
mapAccumLM f a (x:xs) = 
  do
    (a', c) <- f a x
    (a'', cs) <- mapAccumLM f a' xs
    return (a'', c:cs)

relationFileContents :: Set RelationName -> String -> ExceptT RuntimeError IO Relation
relationFileContents parsedRelationNames filename = do
  fileHandle <- liftIO $ openFile filename ReadMode
  contents <- liftIO $ hGetContents fileHandle
  (liftEither) (relationFromSource parsedRelationNames filename contents)

relationFromSource :: Set RelationName -> SourceName -> String -> Either RuntimeError Relation
relationFromSource parsedRelationNames sourceFileName contents =
  do
    let parser = totParser parseRelation
    let source = parse parser sourceFileName contents
    relation <- (first  (ParseError . show) source)
    second (\_ -> relation) (relationChecker parsedRelationNames relation)
    -- TODO hClose fileHandle

relationsDirectory :: String
relationsDirectory = "relations/"