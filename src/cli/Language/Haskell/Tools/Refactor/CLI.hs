{-# LANGUAGE LambdaCase
           , TupleSections
           , FlexibleContexts
           , TypeFamilies
           , StandaloneDeriving
           , RecordWildCards
           #-}
-- | The command line interface for Haskell-tools. It uses the Haskell-tools daemon package starting
-- the daemon in the same process and communicating with it through a channel.
-- It can be used in a one-shot mode, listing all actions in a command-line parameter or using its
-- standard input to perform a series of refactorings.
module Language.Haskell.Tools.Refactor.CLI
  (refactorSession, normalRefactorSession, CLIOptions(..)) where

import Control.Concurrent
import Control.Monad.State.Strict
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Version (showVersion)
import System.Directory
import System.IO

import Language.Haskell.Tools.Daemon
import Language.Haskell.Tools.Daemon.Mode (channelMode)
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Refactor
import Paths_haskell_tools_cli (version)
-- | Normal entry point of the cli.
normalRefactorSession :: [RefactoringChoice IdDom] -> Handle -> Handle -> CLIOptions -> IO Bool
normalRefactorSession refactorings input output options@CLIOptions{..}
  = do hSetBuffering stdout LineBuffering -- to synch our output with GHC's
       hSetBuffering stderr LineBuffering -- to synch our output with GHC's
       refactorSession refactorings
         (\st -> void $ forkIO $ runDaemon refactorings channelMode st
                                   (DaemonOptions False 0 (not cliVerbose) cliNoWatch cliWatchExe))
         input output options

-- | Command-line options for the Haskell-tools CLI
data CLIOptions = CLIOptions { displayVersion :: Bool
                             , cliVerbose :: Bool
                             , executeCommands :: Maybe String
                             , cliNoWatch :: Bool
                             , cliWatchExe :: Maybe FilePath
                             , ghcFlags :: Maybe [String]
                             , packageRoots :: [FilePath]
                             }

-- | Entry point with configurable initialization. Mainly for testing, call 'normalRefactorSession'
-- to use the command-line.
refactorSession :: [RefactoringChoice IdDom] -> ServerInit -> Handle -> Handle -> CLIOptions -> IO Bool
refactorSession _ _ _ output CLIOptions{..} | displayVersion
  = do hPutStrLn output $ showVersion version
       return True
refactorSession refactorings init input output CLIOptions{..} = do
  connStore <- newEmptyMVar
  init connStore
  (recv,send) <- takeMVar connStore -- wait for the server to establish connection
  wd <- getCurrentDirectory
  writeChan send (SetWorkingDir wd)
  case ghcFlags of Just flags -> writeChan send (SetGHCFlags flags)
                   Nothing -> return ()
  writeChan send (AddPackages packageRoots)
  case executeCommands of
    Just cmds -> performCmdOptions refactorings output send (splitOn ";" cmds)
    Nothing -> return ()
  when (isNothing executeCommands) (void $ forkIO $ processUserInput refactorings input output send)
  readFromSocket (isJust executeCommands) output recv

-- | An initialization action for the daemon.
type ServerInit = MVar (Chan ResponseMsg, Chan ClientMessage) -> IO ()

-- | Reads commands from standard input and executes them.
processUserInput :: [RefactoringChoice IdDom] -> Handle -> Handle -> Chan ClientMessage -> IO ()
processUserInput refactorings input output chan = do
  cmd <- hGetLine input
  continue <- processCommand False refactorings output chan cmd
  when continue $ processUserInput refactorings input output chan

-- | Perform a command received from the user. The resulting boolean states if the user may continue
-- (True), or the session is over (False).
processCommand :: Bool -> [RefactoringChoice IdDom] -> Handle -> Chan ClientMessage -> String -> IO Bool
processCommand shutdown refactorings output chan cmd = do
  case splitOn " " cmd of
    ["Exit"] -> writeChan chan Disconnect >> return False
    ["Undo"] -> writeChan chan UndoLast >> return True
    ref : rest | let modPath:selection:details = rest ++ (replicate (2 - length rest) "")
               , ref `elem` refactorCommands refactorings
       -> do writeChan chan (PerformRefactoring ref modPath selection details shutdown False)
             return (not shutdown)
    "Try" : ref : rest | let modPath:selection:details = rest ++ (replicate (2 - length rest) "")
                       , ref `elem` refactorCommands refactorings
       -> do writeChan chan (PerformRefactoring ref modPath selection details shutdown True)
             return (not shutdown)
    _ -> do liftIO $ hPutStrLn output $ "'" ++ cmd ++ "' is not a known command. Commands are: Exit, "
                                            ++ intercalate ", " (refactorCommands refactorings)
            return True

-- | Read the responses of the daemon. The result states if the session exited normally or in an
-- erronous way.
readFromSocket :: Bool -> Handle -> Chan ResponseMsg -> IO Bool
readFromSocket pedantic output recv = do
  continue <- readChan recv >>= processMessage pedantic output
  maybe (readFromSocket pedantic output recv) return continue -- repeate if not stopping

-- | Receives a single response from daemon. Returns Nothing if the execution should continue,
-- Just False on erronous termination and Just True on normal termination.
processMessage :: Bool -> Handle -> ResponseMsg -> IO (Maybe Bool)
processMessage _ output (ErrorMessage msg) = hPutStrLn output msg >> return (Just False)
processMessage pedantic output (CompilationProblem marks)
  = do hPutStrLn output (show marks)
       return (if pedantic then Just False else Nothing)
processMessage _ output (LoadedModules mods)
  = do mapM (\(fp,name) -> hPutStrLn output $ "Loaded module: " ++ name ++ "( " ++ fp ++ ") ") mods
       return Nothing
processMessage _ output (DiffInfo diff)
  = do putStrLn diff
       return Nothing
processMessage _ output (UnusedFlags flags)
  = if not $ null flags
      then do hPutStrLn output $ "Error: The following ghc-flags are not recognized: "
                                    ++ intercalate " " flags
              return $ Just False
      else return Nothing
processMessage _ _ Disconnected = return (Just True)
processMessage _ _ _ = return Nothing

-- | Perform the commands specified by the user as a command line argument.
performCmdOptions :: [RefactoringChoice IdDom] -> Handle -> Chan ClientMessage -> [String] -> IO ()
performCmdOptions refactorings output chan cmds = do
    continue <- mapM (\(shutdown, cmd) -> processCommand shutdown refactorings output chan cmd)
                     (zip lastIsShutdown cmds)
    when (and continue) $ writeChan chan Disconnect
  where lastIsShutdown = replicate (length cmds - 1) False ++ [True]
