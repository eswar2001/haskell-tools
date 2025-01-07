{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Haskell.Tools.Parser.ProgramOptions where

import           Options.Applicative

data Options =
  Options
    { optCommand :: ParseConfig
    }

data ASTRefactors = FunctionDependency | LetRefactoring | RemoveWildCards
  deriving (Show)

data ModuleAction = MERGE | REMOVE | MODIFY
  deriving (Show)

data ParseConfig = Refact ASTParseConfig | SplitAndWrite WriteFileConfig | Module ModuleConfig

data ModuleConfig =
  ModuleConfig {
    sourceModulePath :: String
    , sourceModuleName :: String
    , destinationModulePath :: String
    , destinationModuleName :: String
    , moduleAction :: ModuleAction
  }
  deriving (Show)

data ASTParseConfig =
  ASTParseConfig
    { modulePath      :: String
    , moduleName      :: String
    , functionality   :: ASTRefactors
    }
  deriving (Show)


data WriteFileConfig =
  WriteFileConfig
    { funDeps      :: String
    , clusters     :: String
    , modPath      :: String
    , newModName   :: String
    , modName      :: String
    }
  deriving (Show)


opts :: Parser ParseConfig
opts =
      subparser (command "getFunDeps" $ (info (parserConfig FunctionDependency) (progDesc "Get the function dependency graph of a module")))
  <|> subparser (command "letRefactor" $ (info (parserConfig FunctionDependency) (progDesc "Multiple Let Refactoring")))
  <|> subparser (command "mergeModule" $ (info (modifyModuleConfig MERGE) (progDesc "Merge source to destination")))
  <|> subparser (command "removeModule" $ (info (modifyModuleConfig REMOVE) (progDesc "Remove source from destination")))
  <|> subparser (command "modifyModule" $ (info (modifyModuleConfig MODIFY) (progDesc "modify source in destination")))
  <|> subparser (command "writeModSplits" $ (info writeFileConfig (progDesc "Split the grouped functions into modules")))

modifyModuleConfig :: ModuleAction -> Parser ParseConfig
modifyModuleConfig action =
  Module
  <$> (ModuleConfig
  <$> (strOption (long "source-module-path" <> help "path to which module to read changes from"))
  <*> (strOption (long "destination-module-path" <> help "path to which module to apply changes to"))
  <*> (strOption (long "source-module-name" <> help "module name to read changes from"))
  <*> (strOption (long "destination-module-name" <> help "module name to apply changes to"))
  <*> (pure action))

parserConfig :: ASTRefactors -> Parser ParseConfig
parserConfig refactor =
      Refact
  <$> (ASTParseConfig
  <$> (strOption (long "module-path" <> short 'p' <> help "Path of module to be split"))
  <*> (strOption (long "module-name" <> short 'o' <> help "Module Name"))
  <*> (pure refactor))

writeFileConfig :: Parser ParseConfig
writeFileConfig =
  SplitAndWrite
  <$> (WriteFileConfig
  <$> (strOption (long "fun-deps" <> short 'l' <> help "List of function dependencies"))
  <*> (strOption (long "groups" <> short 'g' <> help "Grouped Functions list"))
  <*> (strOption (long "module-path" <> short 'p' <> help "Path of module to be split"))
  <*> (strOption (long "new-die" <> short 'o' <> help "New directory name for the module"))
  <*> (strOption (long "module-name" <> short 'o' <> help "Module Name")))
