{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Haskell.Tools.Parser.FlowChange where

import Language.Haskell.Tools.Refactor as Refactor hiding (LambdaCase)
import Language.Haskell.Tools.Refactor.Utils.Extensions
import Language.Haskell.Tools.Rewrite.Match.Binds
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar
import GHC hiding (Name, mkModuleName)
import Module as GHC hiding (mkModuleName)
import InstEnv as GHC
import Unify as GHC
import Type as GHC
import Var as GHC
import UniqSupply as GHC
import Unique as GHC
import TysWiredIn as GHC
import TysPrim as GHC
import PrelNames as GHC
import ConLike as GHC
import PatSyn as GHC
import BasicTypes as GHC

import Control.Reference
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List
import qualified Data.Map.Strict as SMap (empty, toList)

import Debug.Trace (trace, traceShowId)
import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances
import Outputable hiding ((<>))
import Language.Haskell.Tools.PrettyPrint

import Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.BackendGHC.GHCUtils (occName, fromSrcText)
import Language.Haskell.Tools.BackendGHC.Names
import Language.Haskell.Tools.BackendGHC.Patterns (trfPattern)
import Language.Haskell.Tools.BackendGHC.Types (trfType)

import Language.Haskell.Tools.AST 
import qualified Language.Haskell.Tools.AST as AST

import Control.Monad.IO.Class

import Language.Haskell.Tools.Refactor
import Control.Monad.IO.Class
import Language.Haskell.Tools.Parser.ParseModule
import GHC.Paths ( libdir )
import Data.Generics.Uniplate.Data ()
import  Language.Haskell.Tools.Rewrite.Match.Exprs
import qualified Data.HashMap.Strict as HM
import System.IO
import Control.Monad
import qualified Data.Aeson as A
import Data.List.Extra (replace)
import Shelly
import Language.Haskell.Tools.Parser.RemoveUnusedFuns (getFunctionDepOfModule')
import qualified Language.Haskell.Tools.Parser.GetFunctionBranching as GFT
import Control.Applicative ((<|>))
import Language.Haskell.Tools.Refactor as HT
import qualified Debug.Trace as Trace
import qualified Data.Text as DT
import Control.Exception

import Language.Haskell.Tools.AST.Representation.Binds (ULocalBind)
import qualified Language.Haskell.Tools.Parser.RemoveUnusedFuns as RS
import GHC.Generics (Generic(..))
import Data.Aeson

data FunctionModified =
  FunctionModified  {
      deleted :: [String]
    , modified :: [String]
    , added :: [String]
    , modName :: String
    }
  deriving (Show,Generic)

instance FromJSON FunctionModified where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON FunctionModified where
  toJSON = genericToJSON defaultOptions

-- listDir :: IO String
-- listDir commit1 commit2 = do
--   shelly $ do
--     "git diff commit1 commit2"

-- defaultFunctionModified =
--     FunctionModified "defaultFile" [] [] []

-- NOTE: Ideally we should add same module functions in FunctionModified
addFunctionModifed funMod funMod2 = FunctionModified (deleted funMod ++ deleted funMod2) (modified funMod ++ modified funMod2) (added funMod ++ added funMod2) (modName funMod)
 
compareFile1 = "Euler.API.Gateway.Gateway.Adyen.Transforms.Refund"
compareFile2 = "Euler.API.Gateway.Gateway.Adyen.Transforms.ModRefund"

compareASTForFuns :: String -> String -> IO ()
compareASTForFuns filePath moduleName' = do
    moduleASTOld <- moduleParser filePath moduleName'
    moduleASTNew <- moduleParser filePath moduleName'
    let oldFuns = HM.fromList $ getAllFunctions moduleASTOld
    let newFuns = HM.fromList $ getAllFunctions moduleASTNew
    let removed = HM.keys $ HM.difference newFuns oldFuns
    -- print removed
    let !y = HM.foldlWithKey (\acc k val ->
                case HM.lookup k newFuns of
                Just newVal -> if ((show val) == (show newVal)) then acc else addFunctionModifed acc (FunctionModified [] [k] [] moduleName')
                Nothing -> addFunctionModifed acc (FunctionModified [k] [] [] moduleName') ) (FunctionModified [] [] removed moduleName') oldFuns
    print y

traverseOverUValBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, Ann UDecl (Dom GhcPs) SrcTemplateStage)
traverseOverUValBind expr@(Ann _ (UValueBinding (Ann _ bind))) = 
    case bind of
        -- Simple bindings like `x = 42`
        USimpleBind pat _ _ -> 
            case getPatternName pat of
                Just name -> Just (name, expr)
                Nothing -> Nothing
                
        -- Function bindings with multiple clauses
        UFunBind matches -> 
            case _annListElems matches of
                (match:_) -> 
                    case getFunctionName match of
                        Just name -> Just (name, expr)
                        Nothing -> Nothing
                [] -> Nothing
  where
    getPatternName :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> Maybe String
    getPatternName (Ann _ pat) = case pat of
        UVarPat (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) -> Just name
        _ -> Nothing
        
    getFunctionName :: Ann UMatch (Dom GhcPs) SrcTemplateStage -> Maybe String
    getFunctionName (Ann _ (UMatch (Ann _ lhs) _ _)) = getLhsName lhs
    
    getLhsName :: UMatchLhs (Dom GhcPs) SrcTemplateStage -> Maybe String
    getLhsName (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) _) = Just name
    getLhsName (UInfixLhs _ (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) _ _) = Just name
    getLhsName _ = Nothing
-- traverseOverUValBind expr@(Ann _ (UTypeSigDecl (Ann _ sig))) =
--     -- Also capture type signatures
--     case _annListElems (_tsName sig) of
--         (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name))))):_) -> Just (name ++ "_type", expr)
--         _ -> Nothing
traverseOverUValBind _ = Nothing

getFunctionNameFromValBind :: Ann UMatch (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionNameFromValBind expr@(Ann _ (UMatch (Ann _ (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) ex1)) _ _)) = Just ex
getFunctionNameFromValBind _ = Nothing

getAllFunctions moduleAST = mapMaybe (\x -> traverseOverUValBind x) (moduleAST ^? biplateRef)


getParentFun :: String -> IO ()
getParentFun modName = do
    let allFiles =  filter (/="Main.hs") $ RS.getAllSubFils "/home/chaitanya/Desktop/work/euler-api-gateway/src/Euler/API/Gateway/Gateway/PineLabs/" []
    print allFiles
    -- x <- mapM (\fileName -> parseAllFiles "Euler.API.Gateway.Gateway.PineLabs.Flows.Emi" (replace "/" "." $ fromMaybe "" $ stripPrefix ("/home/chaitanya/Desktop/work/euler-api-gateway/src/")  fileName)) allFiles
    x <- mapM (\fileName -> parseAllFiles "Euler.API.Gateway.Gateway.PineLabs.Flows.Emi" fileName) allFiles
    print x
    pure ()
    -- moduleAST <- mapM (\x -> moduleParser "/home/chaitanya/Desktop/work/euler-api-gateway/src" modName
    -- let importHM = foldl' (\acc x -> parseImportsAndGetModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])

parseAllFiles :: String -> String -> IO [String]
parseAllFiles modCheck modName = do
    content <- readFile modName
    -- print content
    importsList <- mapM (\x -> if "import " `Data.List.isPrefixOf` x then getImportModuleName x else pure "") (Data.List.lines content)
    let importHM = filter (\x -> (x /= "")) $ filter (\x -> (x == modCheck)) $ importsList
    print importHM
    pure importHM

parseImportsAndGetModule :: _ -> String
parseImportsAndGetModule x@(GHC.ImportDecl _ _ modName pkgQual isBoot _ isQual _ asMod explicitImports) = (GHC.moduleNameString $ unLoc modName)

getModFunctionList :: IO ()
getModFunctionList = do
    let modName = "Euler.API.Gateway.Gateway.Common"
    moduleAST <- moduleParser "/home/chaitanya/Desktop/work/euler-api-gateway/src" modName
    let allFuns = getAllFunctions moduleAST
    !originalList <- mapM (\(k,val) -> do
                    funDeps <- getAllFunctionDeps val k
                    pure $ (k, funDeps)) allFuns

    let updateMods = map (\(k,v) -> (k,(mapWithModName v (HM.fromList allFuns) moduleAST modName))) (originalList)
    -- print importHMQ
    print updateMods
    where
        mapWithModName funList allFuns moduleAST modName = do
            let importHM = foldl' (\acc x -> GFT.parseImportsAndGetModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
            let importHMQ = foldl' (\acc x -> GFT.parseImportsAndGetQualModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
            map (\(fun@(x,y)) -> case (snd <$> HM.lookup (fromMaybe "" x) importHM) <|> (HM.lookup (fromMaybe "" x) importHMQ) of
                                    Just imp -> (imp,y)
                                    Nothing -> (modName,y)) <$> funList


getAllFunctionDeps :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> String -> IO [[(Maybe String, String)]]
getAllFunctionDeps moduleAST funName = do
    let funDepsList =  mapMaybe (\x -> GFT.traverseOverUValBind x funName ) (moduleAST ^? biplateRef)
    let originalList = concat $ map (GFT.getRhsName') (funDepsList ^? biplateRef)
    pure originalList

getFunctionNameFromSimpleBind :: String -> Ann UValueBind (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionNameFromSimpleBind str expr@(Ann _ (USimpleBind pat _ _)) = 
    let name = fromJust $ getPatternName' pat
    in if null name then Nothing else Just $ headMaybe ("getFunctionNameFromSimpleBind " <> str) name
getFunctionNameFromSimpleBind str expr@(Ann _ (UFunBind ex)) =
    let name = mapMaybe getFunctionNameFromValBind (ex ^? biplateRef)
    in if null name then Nothing else Just $ headMaybe ("getFunctionNameFromSimpleBind " <> str) name

headMaybe :: String -> [a] -> a
headMaybe err list = if null list then error $ "HEAD ERROR " <> err else head list

getPatternName' :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getPatternName' expr = Just $ mapMaybe getNamePart (expr ^? biplateRef)

getNamePart :: Ann UName (Dom GhcPs) SrcTemplateStage -> Maybe String
getNamePart expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = Just ex
getNamePart expr = Nothing

getFunctionsCalledInFunction :: Ann URhs (Dom GhcPs) SrcTemplateStage -> [String]
getFunctionsCalledInFunction expr = do
    let !funs = filter (/= "") $ map (getFunctions') (expr ^? biplateRef)
    nub funs

getFunctions' :: Ann UName (Dom GhcPs) SrcTemplateStage -> String
getFunctions' expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = ex
getFunctions' expr = ""

getImportModuleName :: String -> IO ((String))
getImportModuleName importLine = do
    val <- try $ runGhc (Just libdir) $ parseImportDecl importLine
    case val of
        Left (e :: SomeException) -> pure ""
        Right val -> pure $ parseImportsAndGetModule val