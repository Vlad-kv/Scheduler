module Simple_console_executor
    ( runSimpleConsoleExecutor
    ) where

import Project_structure
import Parsers_and_convertors
import Work_with_graph

import System.IO
import System.Environment(getArgs)

import qualified Data.Text.IO as T_IO
import Data.Attoparsec.Text

import qualified Data.Map as M

import qualified Data.Text as T

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Control.Concurrent
import Control.Concurrent.MVar

import System.Process

runSimpleConsoleExecutor :: IO ()
runSimpleConsoleExecutor = do
    res <- runExceptT simpleConsoleExecutor
    case res of
        Left err -> putStrLn $ "error : " ++ err
        _ -> return ()

data SCEEnvironment = SCEEnvironment
    { sCEEnvMVar :: MVar NodeId
    , sCEEnvProj :: Project
    , sCEEnvExp  :: Experiment
    }

simpleConsoleExecutor :: ExceptT String IO String
simpleConsoleExecutor = do
    args <- liftIO getArgs
    filename <- case args of
                    [] -> throwError "Not enough arguments"
                    f : args' -> return f
    content <- liftIO $ T_IO.readFile filename
    project <- case parseOnly projectParser content of
                    Left mess -> throwError $ "Parser failed : " ++ mess
                    Right p -> return p
    experiment <- case M.elems $ projectExperiments project of
                      [] -> throwError "Project has no experiments"
                      e : experiments' -> return e
    let graph = graphFromExperiment experiment
    let tq = createTopologicalQueue graph
    mvar <- liftIO newEmptyMVar
    (_, tq') <- liftIO $ runMainThread mvar project experiment tq

    return $ show tq'

runMainThread :: MVar NodeId -> Project -> Experiment -> TopologicalQueue -> IO ((), TopologicalQueue)
runMainThread mvar project experiment tq = runStateT (runReaderT mainThread SCEEnvironment
                                                { sCEEnvMVar = mvar
                                                , sCEEnvProj = project
                                                , sCEEnvExp  = experiment}) tq

mainThread :: ReaderT SCEEnvironment (StateT TopologicalQueue IO) ()
mainThread = do
    wNode <- state tqTakeFreeNode
    mvar <- asks sCEEnvMVar
    case wNode of
        Nothing ->
            gets tqIsStopped >>= \isStopped ->
            if isStopped then
                return ()
            else
                do
                result <- liftIO $ takeMVar mvar
                liftIO $ putStrLn $ (show result) ++ " finish"
                modify $ tqAcceptNode result
                mainThread
        Just node -> do
            liftIO $ putStrLn $ (show node) ++ " start"
            env <- ask
            threadId <- liftIO $ forkIO $ runReaderT (taskExecutorTread node) env
            mainThread

taskExecutorTread :: NodeId -> ReaderT SCEEnvironment IO ()
taskExecutorTread node = do
    project <- asks sCEEnvProj
    experiment <- asks sCEEnvExp

    let tId = (experimentNodes experiment) M.! node
    let script = taskScript $ (projectTasks project) M.! tId

    exitCode <- liftIO $ system $ T.unpack script
    
    --liftIO $ threadDelay (2 * 10^6)
    mvar <- asks sCEEnvMVar
    liftIO $ putMVar mvar node

