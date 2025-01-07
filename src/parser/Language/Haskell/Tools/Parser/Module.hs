module Language.Haskell.Tools.Parser.Module where

import Language.Haskell.Tools.Parser.ProgramOptions (ModuleConfig(..), ModuleAction(..))
import Language.Haskell.Tools.Parser.ParseModule
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann
-- import Language.Haskell.Tools.AST.Representation.Modules
-- import Language.Haskell.Tools.AST.Representation.Decls
-- import Language.Haskell.Tools.AST.Representation.Names
import Data.List.Extra (replace)
import Control.Monad
import Data.Maybe (mapMaybe)
import System.FilePath
import GHC
import Data.Typeable (cast)

performAction :: ModuleConfig -> IO ()
performAction config = do
    case moduleAction config of
        MERGE -> do
            fromModuleAST <- parseModule' (sourceModulePath config) (sourceModuleName config)
            toModuleAST <- parseModule' (destinationModulePath config) (destinationModuleName config)
            let updatedModuleAST = mergeModule fromModuleAST toModuleAST
            writeUpdatedModule config updatedModuleAST

        REMOVE -> do
            fromModuleAST <- parseModule' (sourceModulePath config) (sourceModuleName config)
            toModuleAST <- parseModule' (destinationModulePath config) (destinationModuleName config)
            let updatedModuleAST = removeModule fromModuleAST toModuleAST
            writeUpdatedModule config updatedModuleAST

        MODIFY -> do
            fromModuleAST <- parseModule' (sourceModulePath config) (sourceModuleName config)
            toModuleAST <- parseModule' (destinationModulePath config) (destinationModuleName config)
            let updatedModuleAST = modifyModule fromModuleAST toModuleAST
            writeUpdatedModule config updatedModuleAST

mergeModule :: Ann UModule (Dom GhcPs) SrcTemplateStage -> Ann UModule (Dom GhcPs) SrcTemplateStage -> Ann UModule (Dom GhcPs) SrcTemplateStage
mergeModule (Ann _ fromMod) (Ann ll toMod) =
    let imports = mergeAnnListG (_modImports fromMod) (_modImports toMod)
        decls = mergeAnnListG (_modDecl fromMod) (_modDecl toMod)
        pragmas = mergeAnnListG (_filePragmas fromMod) (_filePragmas toMod)
    in Ann ll (UModule pragmas (_modHead toMod) imports decls)

removeModule :: Ann UModule (Dom GhcPs) SrcTemplateStage -> Ann UModule (Dom GhcPs) SrcTemplateStage -> Ann UModule (Dom GhcPs) SrcTemplateStage
removeModule (Ann _ fromMod) (Ann ll toMod) =
    let imports = removeAnnListG (_modImports fromMod) (_modImports toMod)
        decls = removeAnnListG (_modDecl fromMod) (_modDecl toMod)
        pragmas = removeAnnListG (_filePragmas fromMod) (_filePragmas toMod)
    in Ann ll (UModule pragmas (_modHead toMod) imports decls)

modifyModule :: Ann UModule (Dom GhcPs) SrcTemplateStage -> Ann UModule (Dom GhcPs) SrcTemplateStage -> Ann UModule (Dom GhcPs) SrcTemplateStage
modifyModule source dest = mergeModule source (removeModule source dest)

pragmaToString :: UFilePragma dom stage -> String
pragmaToString (ULanguagePragma exts) = 
    case _annListElems exts of
        (Ann _ (ULanguageExtension ext):_) -> "LANGUAGE:" ++ ext
        _ -> ""
pragmaToString (UOptionsPragma str) =
    case str of
        Ann _ (UStringNode opt) -> "OPTIONS:" ++ opt

samePragma :: UFilePragma dom stage -> UFilePragma dom stage -> Bool
samePragma p1 p2 = pragmaToString p1 == pragmaToString p2

class HasName e where
    extractName :: e (Dom GhcPs) SrcTemplateStage -> Maybe String

instance HasName UFilePragma where
    extractName pragma = case pragma of
        ULanguagePragma exts -> 
            case _annListElems exts of
                (Ann _ (ULanguageExtension ext):_) -> Just $ "LANGUAGE:" ++ ext
                _ -> Nothing
        UOptionsPragma str ->
            case str of
                Ann _ (UStringNode opt) -> Just $ "OPTIONS:" ++ opt

instance HasName UImportDecl where
    extractName imp = case _importModule imp of
        Ann _ (UModuleName name) -> Just name

instance HasName UDecl where
    extractName decl = case decl of
        UTypeDecl{_declHead = Ann _ (UDeclHead (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))))} -> 
            Just name
        UValueBinding (Ann _ (USimpleBind (Ann _ (UVarPat (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))))) _ _)) -> 
            Just name
        UValueBinding (Ann _ (UFunBind (AnnListG _ ((Ann _ (UMatch (Ann _ (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) _)) _ _)):_)))) -> 
            Just name
        UInstDecl _ (Ann _ rule) _ -> 
            case rule of
                UInstanceRule _ _ (Ann _ (UInstanceHeadCon (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))))) ->
                    Just name
                _ -> Nothing
        _ -> Nothing

mergeAnnListG :: HasName e => AnnListG e (Dom GhcPs) SrcTemplateStage -> AnnListG e (Dom GhcPs) SrcTemplateStage -> AnnListG e (Dom GhcPs) SrcTemplateStage
mergeAnnListG (AnnListG _ source) (AnnListG annot dest) = 
    AnnListG annot (uniqueByName (dest ++ source))

removeAnnListG :: HasName e => AnnListG e (Dom GhcPs) SrcTemplateStage -> AnnListG e (Dom GhcPs) SrcTemplateStage -> AnnListG e (Dom GhcPs) SrcTemplateStage
removeAnnListG (AnnListG _ source) (AnnListG annot dest) = 
    AnnListG annot (removeByName source dest)

uniqueByName :: HasName e => [Ann e (Dom GhcPs) SrcTemplateStage] -> [Ann e (Dom GhcPs) SrcTemplateStage]
uniqueByName [] = []
uniqueByName (x:xs) = x : uniqueByName (filter (not . hasSameName x) xs)

removeByName :: HasName e => [Ann e (Dom GhcPs) SrcTemplateStage] -> [Ann e (Dom GhcPs) SrcTemplateStage] -> [Ann e (Dom GhcPs) SrcTemplateStage]
removeByName source dest = filter (\d -> not $ any (hasSameName d) source) dest

hasSameName :: HasName e => Ann e (Dom GhcPs) SrcTemplateStage -> Ann e (Dom GhcPs) SrcTemplateStage -> Bool
hasSameName (Ann _ e1) (Ann _ e2) = extractName e1 == extractName e2

getImportName :: UImportDecl (Dom GhcPs) SrcTemplateStage -> String
getImportName imp = case _importModule imp of
    Ann _ (UModuleName name) -> name

getTypeDeclName :: UDecl (Dom GhcPs) SrcTemplateStage -> Maybe String
getTypeDeclName UTypeDecl{_declHead = Ann _ (UDeclHead (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))))} = Just name
getTypeDeclName _ = Nothing

getFunctionName' :: UDecl (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionName' (UValueBinding (Ann _ (USimpleBind (Ann _ (UVarPat (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))))) _ _))) = Just name
getFunctionName' (UValueBinding (Ann _ (UFunBind (AnnListG _ ((Ann _ (UMatch (Ann _ (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) _)) _ _)):_))))) = Just name
getFunctionName' _ = Nothing

getInstanceName :: UDecl (Dom GhcPs) SrcTemplateStage -> Maybe String
getInstanceName (UInstDecl _ (Ann _ rule) _) = getInstanceRuleName rule
  where
    getInstanceRuleName (UInstanceRule _ _ (Ann _ head')) = 
        case head' of
            UInstanceHeadCon (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) -> Just name
            _ -> Nothing
getInstanceName _ = Nothing

parseModule' :: FilePath -> String -> IO (Ann UModule (Dom GhcPs) SrcTemplateStage)
parseModule' modulePath moduleName = do
    ast <- moduleParser modulePath moduleName
    putStrLn $ "Parsed module: " ++ moduleName
    pure ast

writeUpdatedModule :: ModuleConfig -> Ann UModule (Dom GhcPs) SrcTemplateStage -> IO ()
writeUpdatedModule config updatedAST = do
    let outputPath = buildOutputPath config
    writeFile outputPath (prettyPrint updatedAST)
    putStrLn $ "Updated module written to: " ++ outputPath

buildOutputPath :: ModuleConfig -> FilePath
buildOutputPath config = 
    destinationModulePath config </> 
    replace "." [pathSeparator] (destinationModuleName config) <.> "hs"