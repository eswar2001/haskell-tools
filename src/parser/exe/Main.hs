{-# LANGUAGE RankNTypes #-}

module Main where

import Language.Haskell.Tools.Parser.ProgramOptions
import Language.Haskell.Tools.Parser.SplitModule
import Language.Haskell.Tools.Parser.Module
import Language.Haskell.Tools.Parser.LetRefactoring
import Language.Haskell.Tools.Parser.RemoveWildCards
import Options.Applicative
import Data.Functor (($>), void)

main :: IO ()
main = do
    parserComb <- execParser (info (opts <**> helper) (fullDesc))
    handleCommand parserComb

-- Handling different command cases
handleCommand :: ParseConfig -> IO ()
handleCommand (Module moduleConfig) = handleModuleAction moduleConfig
handleCommand (Refact astParseConfig) = handleRefactor astParseConfig
handleCommand (SplitAndWrite writeFileConfig) = handleSplitAndWrite writeFileConfig

-- Handling module actions
handleModuleAction :: ModuleConfig -> IO ()
handleModuleAction moduleConfig = do
  print moduleConfig
  performAction moduleConfig

-- Handling refactor actions
handleRefactor :: ASTParseConfig -> IO ()
handleRefactor astParseConfig = case functionality astParseConfig of
  FunctionDependency -> getAndPrintFunctionDeps astParseConfig
  LetRefactoring    -> refactorLet astParseConfig
  RemoveWildCards   -> removeWildcards astParseConfig

-- Get function dependencies and print them
getAndPrintFunctionDeps :: ASTParseConfig -> IO ()
getAndPrintFunctionDeps astParseConfig = do
  deps <- getFunctionDeps (modulePath astParseConfig) (moduleName astParseConfig)
  print deps

-- Perform let refactoring
refactorLet :: ASTParseConfig -> IO ()
refactorLet astParseConfig = removeMultiLets (modulePath astParseConfig) (moduleName astParseConfig) $> ()

-- Remove wildcards
removeWildcards :: ASTParseConfig -> IO ()
removeWildcards astParseConfig = removeWildCards (modulePath astParseConfig) (moduleName astParseConfig) $> ()

-- Handle the split and write functionality
handleSplitAndWrite :: WriteFileConfig -> IO ()
handleSplitAndWrite writeFileConfig = void $ splitAndWrite 
  (modPath writeFileConfig)
  (modName writeFileConfig)
  (clusters writeFileConfig)
  (funDeps writeFileConfig)