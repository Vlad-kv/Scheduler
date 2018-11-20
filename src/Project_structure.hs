module Project_structure
    ( Project(..)
    , Experiment(..)
    , ExperimentId(..)
    , Edge(..)
    , NodeId(..)
    , Task(..)
    , TaskId(..)
    , TaskParameter(..)
    ) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import Data.Text (Text, pack)

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
    , taskScript     :: Text
    } deriving (Eq, Show)

newtype ExperimentId = ExperimentId Integer deriving (Eq)
instance Show ExperimentId where
    show (ExperimentId id) = show id

newtype NodeId = NodeId Integer deriving (Eq, Ord)
instance Show NodeId where
    show (NodeId id) = show id

data Edge = Edge
    { edgeSrc  :: NodeId
    , edgeDest :: NodeId
    } deriving (Eq)

instance Show Edge where
    show e = (show $ edgeSrc e) ++ " -> " ++ (show $ edgeDest e)

data Experiment = Experiment
    { experimentName   :: Text
    , experimentId     :: ExperimentId
    , experimentParent :: Maybe ExperimentId
    , experimentNodes  :: Map NodeId TaskId
    , experimentEdges  :: [Edge]
    } deriving (Eq, Show)

data Project = Project
    { projectName        :: Text
    , projectTasks       :: [Task]
    , projectExperiments :: [Experiment]
    } deriving (Eq, Show)
