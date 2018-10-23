{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs        #-}

module Lib
    ( projectParser
    , projectToText
    ) where

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Text (Text, pack)
import qualified Data.Text as T
import Control.Applicative

data TaskParameter = TaskParameter
    { taskParameterName  :: Text
    , taskParameterValue :: Text
    } deriving (Eq, Show)

newtype TaskId = TaskId Integer deriving (Eq)
instance Show TaskId where
    show (TaskId id) = show id

data Task = Task
    { taskName       :: Text
    , taskId         :: TaskId
    , taskParameters :: [TaskParameter]
    , taskParent     :: Maybe TaskId
    } deriving (Eq, Show)

newtype ExperimentId = ExperimentId Integer deriving (Eq)
instance Show ExperimentId where
    show (ExperimentId id) = show id

data Edge = Edge
    { edgeSrc  :: TaskId
    , edgeDest :: TaskId
    } deriving (Eq)

instance Show Edge where
    show e = (show $ edgeSrc e) ++ " -> " ++ (show $ edgeDest e)

data Experiment = Experiment
    { experimentName   :: Text
    , experimentId     :: ExperimentId
    , experimentParent :: Maybe ExperimentId
    , experimentGraph  :: [Edge]
    } deriving (Eq, Show)

data Project = Project
    { projectName        :: Text
    , projectTasks       :: [Task]
    , projectExperiments :: [Experiment]
    } deriving (Eq, Show)

spaceSymbol :: Parser ()
spaceSymbol = satisfy (inClass " \t\n\r") >> pure ()

skipManySpaces :: Parser ()
skipManySpaces  = skipMany  spaceSymbol

skipMany1Spaces :: Parser ()
skipMany1Spaces = skipMany1 spaceSymbol

quotedText :: Parser Text
quotedText = do
    skipManySpaces
    char '"'
    str <- P.takeWhile $ (/=) '"'
    char '"'
    return str

taskParameterParser :: Parser TaskParameter
taskParameterParser = do
    skipManySpaces
    string "Param"
    skipMany1Spaces
    name  <- quotedText
    value <- quotedText
    return TaskParameter
            { taskParameterName  = name
            , taskParameterValue = value
            }

parentParser :: Parser (Maybe Integer)
parentParser = (<|>) (skipManySpaces >> string "Parent" >> skipMany1Spaces >> decimal >>= \val -> return (Just val))
                     (return Nothing)

taskParser :: Parser Task
taskParser = do
    skipManySpaces
    string "Task"
    skipMany1Spaces
    name       <- quotedText
    skipManySpaces
    id         <- decimal
    parent     <- parentParser
    parameters <- many' taskParameterParser
    return Task
            { taskName       = name
            , taskId         = TaskId id
            , taskParameters = parameters
            , taskParent     = TaskId <$> parent
            }

edgeParser :: Parser Edge
edgeParser = do
    skipManySpaces
    s <- decimal
    skipManySpaces
    string "->"
    skipManySpaces
    d <- decimal
    return Edge
            { edgeSrc  = TaskId s
            , edgeDest = TaskId d
            }

experimentParser :: Parser Experiment
experimentParser = do
    skipManySpaces
    string "Experiment"
    skipMany1Spaces
    name       <- quotedText
    skipManySpaces
    id         <- decimal
    parent     <- parentParser
    graph      <- many' edgeParser
    return Experiment
            { experimentName   = name
            , experimentId     = ExperimentId id
            , experimentParent = ExperimentId <$> parent
            , experimentGraph  = graph
            }

projectParser :: Parser Project
projectParser = do
    string "Project"
    skipMany1Spaces
    name       <- quotedText
    tasks      <- many' taskParser
    experimens <- many' experimentParser
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
taskToText t = T.concat $ [ "Task \"", taskName t, "\" "
                          , pack $ show $ taskId t, "\n"
                          , showParent $ taskParent t]
                          ++ (map taskParameterToText $ taskParameters t)

experimentToText :: Experiment -> Text
experimentToText e = T.concat $ [ "Experiment \"", experimentName e, "\" ", pack $ show $ experimentId e, "\n"
                                , showParent $ experimentParent e]
                                ++ (map (\edge -> pack $ "    " ++ (show edge) ++ "\n") $ experimentGraph e)

projectToText :: Project -> Text
projectToText proj = T.concat $ "Project \"" : projectName proj : "\"\n"
                                :  (map taskToText $ projectTasks proj)
                                ++ (map experimentToText $ projectExperiments proj)

