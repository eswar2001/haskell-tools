{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
module Language.Haskell.Tools.Parser.LLMDataSetGeneration where

import GHC.Generics
import Control.Concurrent (threadDelay)
import Control.Exception
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Parser.ParseModule
import Control.Monad (forM,when)
import Data.Maybe (isJust,fromJust,fromMaybe,catMaybes)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeExtension, (</>))
import System.IO
import qualified Data.List
import Data.List.Extra (nub,find,splitOn,replace,isInfixOf)
import Data.Aeson
import Language.Haskell.Tools.PrettyPrint
import Outputable (Outputable(..), showSDocUnsafe, cat)
import Data.ByteString.Lazy (writeFile)
import Debug.Trace
import Data.Data
import GHC hiding (loadModule)
import qualified GHC
import Outputable (Outputable(..), showSDocUnsafe, cat)
import GHC.Paths ( libdir )
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Extra (splitOn,trim,replace, cons)
import GHC.LanguageExtensions
import Control.Concurrent
import SrcLoc (noSrcSpan, combineSrcSpans)
import DynFlags
import Language.Haskell.Tools.BackendGHC
import Language.Haskell.Tools.PrettyPrint.Prepare
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Decls (trfDecls, trfDeclsGroup)
import Language.Haskell.Tools.BackendGHC.Exprs (trfText')
import Language.Haskell.Tools.BackendGHC.Names (TransformName, trfName)
import Language.Haskell.Tools.BackendGHC.Modules hiding (trfModuleHead)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Parser.ParseModule
import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances
import qualified Data.Text as T
import qualified Data.Char as Char

isHaskellFile :: FilePath -> Bool
isHaskellFile file = takeExtension file == ".hs"

getAllHaskellModules :: FilePath -> IO [FilePath]
getAllHaskellModules dir = do
  contents <- getDirectoryContents dir
  let files = filter (\f -> f /= "." && f /= "..") contents
  paths <- forM files $ \file -> do
    let path = dir System.FilePath.</> file
    isDir <- doesDirectoryExist path
    if isDir && (not $ any (`isInfixOf` path) ["/tests/","/dist-","/test/",".stack-work",".git"])
      then getAllHaskellModules path
      else return [path | isHaskellFile path]
  return (concat paths)

getModuleName :: String -> Maybe String
getModuleName content = do
    let moduleLine = find ("module" `Data.List.isPrefixOf`) (lines content)
    (Just . head . splitOn " " . replace "module " "" . replace " where" "") =<< moduleLine

removeIfNothing :: (String, Maybe String) -> Bool
removeIfNothing (_, second) = (isJust second)

getFileContent :: String -> IO String
getFileContent filePath = (evaluate) =<< readFile filePath

listHaskellFilesInDir :: String -> IO ()
listHaskellFilesInDir dir = do
    modules <- getAllHaskellModules dir
    moduleNames <- forM modules $ \modulePath -> do
        threadDelay 100
        print ("reading file: " <> modulePath)
        contents <- getFileContent modulePath
        pure $ getModuleName contents
    modulesAsJsonParquetRow <- forM (filter removeIfNothing $ zip modules moduleNames)
                    $ \(modulePath,moduleName) -> do
                        res <- try $ moduleParser modulePath (fromJust moduleName)
                        case res of
                            Right (modAST :: (Ann AST.UModule (Dom GhcPs) SrcTemplateStage)) -> do
                                let moduleRow = createJsonParquetRow (prettyPrint modAST) modulePath MODULE
                                case modAST of
                                    (Ann _ (UModule (AnnListG _ filePragmas) (AnnMaybeG _ (Just (Ann _ (UModuleHead name pragma exports)))) (AnnListG _ modImports) (AnnListG _ modDecls))) ->
                                        let modNameRow = createJsonParquetRow (prettyPrint name) modulePath NAME
                                            headPragmaRow = createJsonParquetRow (prettyPrint pragma) modulePath PRAGMA
                                            headExportRow = createJsonParquetRow (prettyPrint exports) modulePath EXPORT
                                            filePragmasRows = map (\x -> createJsonParquetRow (prettyPrint x) modulePath PRAGMA) filePragmas
                                            modImportsRows = map (\x -> createJsonParquetRow (prettyPrint x) modulePath IMPORTS) modImports
                                            declRows = map (\x -> createJsonParquetRow (prettyPrint x) modulePath FUNCTION_TYPE_CLASS) modDecls
                                        in pure $ Just $ [modNameRow,headPragmaRow,headExportRow,moduleRow] <> filePragmasRows <> modImportsRows <> declRows
                                    (Ann _ (UModule (AnnListG _ filePragmas) _ (AnnListG _ modImports) (AnnListG _ modDecls))) ->
                                        let filePragmasRows = map (\x -> createJsonParquetRow (prettyPrint x) modulePath PRAGMA) filePragmas
                                            modImportsRows = map (\x -> createJsonParquetRow (prettyPrint x) modulePath IMPORTS) modImports
                                            declRows = map (\x -> createJsonParquetRow (prettyPrint x) modulePath FUNCTION_TYPE_CLASS) modDecls
                                        in pure $ Just $ [moduleRow] <> filePragmasRows <> modImportsRows <> declRows
                                    _ -> pure $ Just [moduleRow]
                                    _ -> pure $ Just [moduleRow]
                            Left (err :: SomeException) -> appendFile "error.log" ((show err) <> " module_path: " <> modulePath <> " : "<> (fromMaybe "" moduleName)) *> pure Nothing
    Data.ByteString.Lazy.writeFile "data.json" (encode $ concat $ catMaybes $ modulesAsJsonParquetRow)

replaceMultipleNewlines :: String -> String
replaceMultipleNewlines = concat . map (\group -> if head group == '\n' then "\n" else group) . group

createJsonParquetRow :: String -> String -> AST_TYPE -> Maybe JSON_PARQUET_ROW
createJsonParquetRow code file_name ast_type =
    let code_g = replaceMultipleNewlines code
    in if length code_g > 1
                then Just $ JSON_PARQUET_ROW {
                        code = code_g
                        , file_name
                        , size = length $ code_g
                        , ast_type
                        , spaces = Data.List.foldl' (\acc x -> if x == ' ' then acc + 1 else acc) 0 (code_g)
                        , tabs = Data.List.foldl' (\acc x -> if x == '\t' then acc + 1 else acc) 0 (code_g)
                        , new_line = Data.List.foldl' (\acc x -> if x == '\n' then acc + 1 else acc) 0 (code_g)
                    }
            else Nothing

data AST_TYPE = FUNCTION_TYPE_CLASS | IMPORTS | MODULE | NAME | PRAGMA | COMMENTS | EXPORT
  deriving stock (Generic,Show)
  deriving anyclass (ToJSON)

data JSON_PARQUET_ROW = JSON_PARQUET_ROW {
    code :: String
    , file_name :: String
    , size :: Int
    , ast_type :: AST_TYPE
    , spaces :: Int
    , tabs :: Int
    , new_line :: Int
}
  deriving stock (Generic,Show)
  deriving anyclass (ToJSON)