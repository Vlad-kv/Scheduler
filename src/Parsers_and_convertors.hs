{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module Parsers_and_convertors
    ( projectParser
    , projectToText
    ) where

import Project_structure;

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Text (Text, pack)
import qualified Data.Text as T
import Control.Applicative

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

spaceSymbol :: Parser ()
spaceSymbol = satisfy (inClass " \t\n\r") >> pure ()

skipManySpaces :: Parser ()
skipManySpaces  = skipMany  spaceSymbol

skipMany1Spaces :: Parser ()
skipMany1Spaces = skipMany1 spaceSymbol

quotedTextWithoutQuotes :: Parser Text
quotedTextWithoutQuotes = do
    skipManySpaces >> char '\"'
    str <- P.takeWhile $ (/=) '\"'
    char '\"'
    return str

quotedText :: Parser Text
quotedText =
    skipManySpaces >> char '"' >>
    let takeQuotedChar = notChar '\"' >>= \c ->
                            case c of
                                '\\' -> anyChar
                                _    -> pure c
    in
    let textPart = takeQuotedChar >>= \c -> (P.takeWhile $ not . inClass "\\\"") >>=
            \text -> pure $ T.cons c text
    in
    do
    strings <- many' textPart
    char '\"'
    return $ T.concat strings

screenText :: Text -> Text
screenText = T.concatMap (\c -> if (inClass "\\\"" c) then pack ['\\', c]
                                else T.singleton c)

taskParameterParser :: Parser TaskParameter
taskParameterParser = do
    skipManySpaces >> string "Param" >> skipMany1Spaces
    name  <- quotedTextWithoutQuotes
    value <- quotedTextWithoutQuotes
    return TaskParameter
            { taskParameterName  = name
            , taskParameterValue = value
            }

parentParser :: Parser (Maybe Integer)
parentParser = (<|>) (skipManySpaces >> string "Parent" >> skipMany1Spaces >> decimal >>= \val -> return (Just val))
                     (return Nothing)

taskParser :: Parser Task
taskParser = do
    skipManySpaces >> string "Task" >> skipMany1Spaces
    name       <- quotedTextWithoutQuotes
    skipManySpaces
    id         <- decimal
    skipManySpaces >> string "Script" >> skipMany1Spaces
    script     <- quotedText
    parent     <- parentParser
    parameters <- many' taskParameterParser
    return Task
            { taskName       = name
            , taskId         = TaskId id
            , taskParameters = parameters
            , taskParent     = TaskId <$> parent
            , taskScript     = script
            }

edgeParser :: Parser Edge
edgeParser = do
    skipManySpaces
    s <- decimal
    skipManySpaces >> string "->" >> skipManySpaces
    d <- decimal
    return Edge
            { edgeSrc  = NodeId s
            , edgeDest = NodeId d
            }

nodesParser :: Parser (Map NodeId TaskId)
nodesParser = do
    skipManySpaces
    char '['
    l <- (skipManySpaces >> char ']' >> return []) <|>
            (skipManySpaces >> decimal >>= \first ->
                (many' $ skipManySpaces >> char ',' >> skipManySpaces >> decimal) >>= \t ->
                    skipManySpaces >>
                    char ']' >>
                    return (first : t)
            )
    return $ M.fromList $ zip (fmap NodeId [1..]) (fmap TaskId l)

experimentParser :: Parser Experiment
experimentParser = do
    skipManySpaces >> string "Experiment" >> skipMany1Spaces
    name   <- quotedTextWithoutQuotes
    skipManySpaces
    id     <- decimal
    parent <- parentParser
    nodes  <- nodesParser
    graph  <- many' edgeParser
    return Experiment
            { experimentName   = name
            , experimentId     = ExperimentId id
            , experimentParent = ExperimentId <$> parent
            , experimentNodes  = nodes
            , experimentGraph  = graph
            }

projectParser :: Parser Project
projectParser = do
    string "Project" >> skipMany1Spaces
    name       <- quotedTextWithoutQuotes
    tasks      <- many' taskParser
    experimens <- many' experimentParser
    skipManySpaces
    endOfInput
    return Project
            { projectName        = name
            , projectTasks       = tasks
            , projectExperiments = experimens
            }

taskParameterToText :: TaskParameter -> Text
taskParameterToText t = T.concat $ [ "    Param "
                                   , pack $ show $ taskParameterName t
                                   , " "
                                   , pack $ show $ taskParameterValue t
                                   , "\n"]

showParent :: Show a => Maybe a -> Text
showParent m = case m of Just parent -> pack $ "    Parent " ++ (show parent) ++ "\n"
                         _           -> ""

taskToText :: Task -> Text
taskToText t = T.concat $ [ "Task \"", taskName t, "\" ", pack $ show $ taskId t, "\n"
                          , "    Script \"", screenText $ taskScript t, "\"\n"
                          , showParent $ taskParent t]
                          ++ (map taskParameterToText $ taskParameters t)

experimentToText :: Experiment -> Text
experimentToText e = T.concat $ [ "Experiment \"", experimentName e, "\" ", pack $ show $ experimentId e, "\n"
                                , showParent $ experimentParent e
                                , let (_, ids) = unzip $ M.toList $ experimentNodes e in
                                  pack $ "    " ++ (show ids) ++ "\n"]
                                ++ (map (\edge -> pack $ "    " ++ (show edge) ++ "\n") $ experimentGraph e)

projectToText :: Project -> Text
projectToText proj = T.concat $ "Project \"" : projectName proj : "\"\n"
                                :  (map taskToText $ projectTasks proj)
                                ++ (map experimentToText $ projectExperiments proj)
