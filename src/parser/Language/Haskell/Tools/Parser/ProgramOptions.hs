{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Haskell.Tools.Parser.ProgramOptions where

import           Options.Applicative
import           Control.Monad (void)

data Options =
  Options
    { optCommand :: ParseConfig
    }

data ASTRefactors = FunctionDependency | LetRefactoring | RemoveWildCards
  deriving (Show)

data ModuleAction = MERGE | REMOVE | MODIFY
  deriving (Show)

data ParseConfig = Refact ASTParseConfig | SplitAndWrite WriteFileConfig | Module ModuleConfig
  deriving (Show)

data ModuleConfig =
  ModuleConfig
    { sourceModulePath    :: String
    , sourceModuleName    :: String
    , destinationModulePath :: String
    , destinationModuleName :: String
    , moduleAction        :: ModuleAction
    }
  deriving (Show)

data ASTParseConfig =
  ASTParseConfig
    { modulePath    :: String
    , moduleName    :: String
    , functionality :: ASTRefactors
    }
  deriving (Show)

data WriteFileConfig =
  WriteFileConfig
    { funDeps        :: String
    , clusters       :: String
    , modPath        :: String
    , newModName     :: String
    , modName        :: String
    }
  deriving (Show)

-- The main options parser
opts :: Parser ParseConfig
opts = subparser
  (  command "getFunDeps"     (info (parserConfig FunctionDependency) (progDesc "Get the function dependency graph of a module"))
  <> command "letRefactor"     (info (parserConfig LetRefactoring) (progDesc "Multiple Let Refactoring"))
  <> command "removeWildCards" (info (parserConfig RemoveWildCards) (progDesc "Remove wildcards in a module"))
  <> command "mergeModule"     (info (modifyModuleConfig MERGE) (progDesc "Merge source to destination"))
  <> command "removeModule"    (info (modifyModuleConfig REMOVE) (progDesc "Remove source from destination"))
  <> command "modifyModule"    (info (modifyModuleConfig MODIFY) (progDesc "Modify source in destination"))
  <> command "writeModSplits"  (info writeFileConfig (progDesc "Split the grouped functions into modules"))
  )

-- Subparser for modifying module configurations
modifyModuleConfig :: ModuleAction -> Parser ParseConfig
modifyModuleConfig action = Module <$> (ModuleConfig
  <$> strOption (long "source-module-path" <> help "Path to read changes from")
  <*> strOption (long "source-module-name" <> help "Name of the source module")
  <*> strOption (long "destination-module-path" <> help "Path to apply changes to")
  <*> strOption (long "destination-module-name" <> help "Name of the destination module")
  <*> pure action)

-- Subparser for refactoring options (function dependency, let refactoring, etc.)
parserConfig :: ASTRefactors -> Parser ParseConfig
parserConfig refactor = Refact <$> (ASTParseConfig
  <$> strOption (long "module-path" <> short 'p' <> help "Path of the module to refactor")
  <*> strOption (long "module-name" <> short 'o' <> help "Module name")
  <*> pure refactor)

-- Parser for splitting and writing modules
writeFileConfig :: Parser ParseConfig
writeFileConfig = SplitAndWrite <$> (WriteFileConfig
  <$> strOption (long "fun-deps" <> short 'l' <> help "Function dependencies")
  <*> strOption (long "groups" <> short 'g' <> help "Grouped functions")
  <*> strOption (long "module-path" <> short 'p' <> help "Path to the module to split")
  <*> strOption (long "new-die" <> short 'o' <> help "New directory name for the module")
  <*> strOption (long "module-name" <> short 'o' <> help "Module name"))
