module Language.Haskell.Tools.Parser.Module where

import Language.Haskell.Tools.Parser.ProgramOptions (ModuleConfig(..),ModuleAction(..))
import Language.Haskell.Tools.Parser.ParseModule
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Data.List.Extra (replace)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint
import Data.List
import GHC
import Control.Reference

combineModules
    (Ann l fromModuleAST@(UModule s_filePragmas s_modHead s_modImports s_modDecl))
    (Ann ll toModuleAST@(UModule d_filePragmas d_modHead d_modImports d_modDecl))
        =
            let filePragmas = mergeAnnListG s_filePragmas d_filePragmas
                imports = mergeAnnListG s_modImports d_modImports
                decls = mergeAnnListG s_modDecl d_modDecl
                modHead = mergeUModuleHead s_modHead d_modHead
            in Ann ll (UModule filePragmas d_modHead imports decls)

mergeUModuleHead (AnnMaybeG _ (Just (Ann _ a))) (AnnMaybeG opt (Just (Ann l b))) = AnnMaybeG opt (Just $ Ann l $ UModuleHead {
    _mhName = _mhName b,  -- Keep the destination's name
    _mhPragma = (_mhPragma b), --mergeMaybe (_mhPragma a)
    _mhExports = mergeAnnMaybeGExports (_mhExports a) (_mhExports b)
    })
    where
        mergeAnnMaybeGExports ((AnnMaybeG _ (Just a))) ((AnnMaybeG ll (Just b))) = (AnnMaybeG ll $ Just $ mergeExportSpecs a b)
        mergeAnnMaybeGExports _ a = a

        mergeExportSpecs (Ann _ s) (Ann l d) = Ann l $ UExportSpecs {
            _espExports = mergeAnnListG (_espExports s) (_espExports d)
        }

mergeAnnListG s d = AnnListG (_annListAnnot d) ((_annListElems s) <> (_annListElems d))

performAction :: ModuleConfig -> IO ()
performAction moduleConfig =
    case moduleAction moduleConfig of
        MERGE -> do
            fromModuleAST <- parseModule' (sourceModulePath moduleConfig) (sourceModuleName moduleConfig)
            toModuleAST <- parseModule' (destinationModulePath moduleConfig) (destinationModuleName moduleConfig)
            let updatedModuleAST = combineModules fromModuleAST toModuleAST
            writeFile ((destinationModulePath moduleConfig) <> (replace "." "/" (destinationModuleName moduleConfig)) <> ".hs") (prettyPrint updatedModuleAST)
            pure ()
        REMOVE -> pure ()
        MODIFY -> pure ()

parseModule' moduleName modulePath = do
    ast <- moduleParser modulePath moduleName
    print ("parsed module " <> moduleName)
    pure ast