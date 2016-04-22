{-# LANGUAGE FlexibleContexts 
           , LambdaCase 
           , RankNTypes 
           , ScopedTypeVariables 
           #-}

-- | Helper functions for using the AST.
module Language.Haskell.Tools.AST.Helpers where

import SrcLoc
import qualified Name as GHC

import Control.Reference hiding (element)
import Data.List
import Data.Maybe
import Data.Function hiding ((&))
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Modules
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.References

import Debug.Trace

ordByOccurrence :: Name a -> Name a -> Ordering
ordByOccurrence = compare `on` nameElements

-- | The occurrence of the name.
nameString :: Name a -> String
nameString = intercalate "." . nameElements

-- | The qualifiers and the unqualified name
nameElements :: Name a -> [String]
nameElements n = (n ^? qualifiers&annList&element&simpleNameStr) 
                    ++ [n ^. unqualifiedName&element&simpleNameStr]

-- | The qualifier of the name
nameQualifier :: Name a -> [String]
nameQualifier n = n ^? qualifiers&annList&element&simpleNameStr
         
-- | Does the import declaration import only the explicitly listed elements?
importIsExact :: ImportDecl a -> Bool
importIsExact = isJust . (^? importSpec&annJust&element&importSpecList)  
  
-- | Does the import declaration has a 'hiding' clause?
importIsHiding :: ImportDecl a -> Bool
importIsHiding = isJust . (^? importSpec&annJust&element&importSpecHiding)
       
-- | All elements that are explicitly listed to be imported in the import declaration
importExacts :: Simple Traversal (ImportDecl a) (IESpec a)
importExacts = importSpec&annJust&element&importSpecList&annList&element

-- | All elements that are hidden in an import
importHidings :: Simple Traversal (ImportDecl a) (IESpec a)
importHidings = importSpec&annJust&element&importSpecList&annList&element
         
-- | Possible qualifiers to use imported definitions         
importQualifiers :: ImportDecl a -> [[String]]
importQualifiers imp 
  = (if isAnnNothing (imp ^. importQualified) then [[]] else [])
      ++ maybe [] (\n -> [nameElements n]) 
               (imp ^? importAs&annJust&element&importRename&element)
        
bindingName :: Simple Traversal (Ann ValueBind (NodeInfo (SemanticInfo n) s)) n
bindingName = element&(valBindPat&element&patternName &+& funBindMatches&annList&element&matchName)
                     &semantics&nameInfo
                     
declHeadNames :: Simple Traversal (Ann DeclHead a) (Ann Name a)
declHeadNames = element & (dhName &+& dhBody&declHeadNames &+& dhAppFun&declHeadNames)

               
typeParams :: Simple Traversal (Ann Type a) (Ann Type a)
typeParams = fromTraversal typeParamsTrav
  where typeParamsTrav f (Ann a (TyFun p r)) = Ann a <$> (TyFun <$> f p <*> typeParamsTrav f r)
        typeParamsTrav f (Ann a (TyForall vs ctx t)) = Ann a <$> (TyForall vs ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyCtx ctx t)) = Ann a <$> (TyCtx ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyParen t)) = Ann a <$> (TyParen <$> typeParamsTrav f t)
        typeParamsTrav f t = f t
        

        
semantics :: Simple Lens (Ann a (NodeInfo sema src)) sema
semantics = annotation&semanticInfo

-- | A class for accessing names that identify the declared program element.
-- This is implemented for performance, it could be done with generics.
-- class HasDeclaredNames d where
--   allDeclaredNames :: Simple Traversal (d a) (Ann Name a)
--   allDeclaredNames = mainDeclaredName &+& subDeclaredNames

--   mainDeclaredName :: Simple Partial (d a) (Ann Name a)
--   subDeclaredNames :: Simple Traversal (d a) (Ann Name a)

-- instance HasDeclaredNames a => HasDeclaredNames (Ann a) where
--   mainDeclaredName = element & mainDeclaredName
--   subDeclaredNames = element & subDeclaredNames

-- instance HasDeclaredNames Decl where
--   mainDeclaredName = declHead & declHeadNames 
--                        &+& declTypeFamily & element & tfHead & declHeadNames
--                        &+& declPatSyn & element & patName
--                        &+& declValBind & bindingName
--   subDeclaredNames = declCons & annList & allDeclaredNames

-- instance HasDeclaredNames ConDecl where
--   mainDeclaredName = conDeclName
--   subDeclaredNames = conDeclFields & annList & allDeclaredNames

-- instance HasDeclaredNames FieldDecl where
--   mainDeclaredName = emptyRef
--   subDeclaredNames = fieldNames & annList

-- instance HasDeclaredNames ValueBind where
--   mainDeclaredName = valBindPat & element & patternVar &+& funBindMatches & annList & mainDeclaredName
--   subDeclaredNames = valBindLocals & annList & allDeclaredNames
--                        &+& valBindRhs & allDeclaredNames
--                        &+& funBindMatches & annList & subDeclaredNames

-- instance HasDeclaredNames Match where
--   mainDeclaredName = matchName
--   subDeclaredNames = matchArgs & allDeclaredNames 
--                        &+& matchRhs & allDeclaredNames 
--                        &+& matchBinds & allDeclaredNames

-- instance HasDeclaredNames Rhs where
--   mainDeclaredName = emptyRef
--   subDeclaredNames = rhsExpr & subDeclaredNames &+& rhsGuards & annList & subDeclaredNames

-- instance HasDeclaredNames Expr where
--   mainDeclaredName = emptyRef
--   subDeclaredNames = exprBindings & annList & allDeclaredNames
--                        &+& procPattern & allDeclaredNames
--                        &+& exprFunBind & annList & allDeclaredNames
--                        &+& exprAlts & annList & allDeclaredNames
--                        &+& innerExprs & subDeclaredNames
--                        &+& tupleSectionElems & annList
--                        &+& compBody & annList
--     where innerExprs = exprLhs &+& exprRhs &+& exprFun &+& exprArg &+& exprInner &+& exprCond &+& exprThen &+& exprElse &+& exprCase
--                          &+& tupleElems & annList &+& listElems & annList &+& enumFrom &+& enumTo & annJust &+& enumThen & annJust
--                          &+& compExpr

-- instance HasDeclaredNames (Stmt' expr) where
--   mainDeclaredName = emptyRef
--   subDeclaredNames = stmtPattern & allDeclaredNames 
--                        &+& stmtExpr & subDeclaredNames
--                        &+& stmtBinds & annList & allDeclaredNames

-- instance HasDeclaredNames (Alt' expr) where
--   mainDeclaredName = emptyRef
--   subDeclaredNames = altPattern & allDeclaredNames
--                        &+& altRhs & allDeclaredNames
--                        &+& altBinds & annJust & localBinds & annList & allDeclaredNames

-- instance HasDeclaredNames ListCompBody where
--   mainDeclaredName = emptyRef
--   subDeclaredNames = compStmts & annList & allDeclaredNames


-- instance HasDeclaredNames Pattern where
--   mainDeclaredName = patternVar
--   subDeclaredNames = (patternLhs &+& patternRhs &+& patternArg &+& patternElems & annList &+& patternInner) & allDeclaredNames
--                        &+& patternFields & allDeclaredNames

-- instance HasDeclaredNames PatternFields where
--   mainDeclaredName = fieldPatternName
--   subDeclaredNames = emptyRef


getTopLevelDeclName :: Decl (NodeInfo (SemanticInfo n) src) -> Maybe n
getTopLevelDeclName (d @ TypeDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ TypeFamilyDecl {}) = listToMaybe (d ^? declTypeFamily & element & tfHead & dhNames)
getTopLevelDeclName (d @ ClosedTypeFamilyDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ DataDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ GDataDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ ClassDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ PatternSynonymDecl {}) = d ^? declPatSyn & element & patName & semantics & nameInfo
getTopLevelDeclName (d @ ValueBinding {}) = listToMaybe (d ^? declValBind & bindingName)
getTopLevelDeclName _ = Nothing

-- getAllDeclNames :: Decl (NodeInfo (SemanticInfo n) src) -> [n]

-- getClassElementDeclName :: ClassElement (NodeInfo (SemanticInfo n) src) -> [n]
-- getClassElementDeclName (d @ ClsSig {}) = d ^? ceTypeSig & element & tsName & annList & semantics & nameInfo
-- getClassElementDeclName (d @ ClsTypeFam {}) = d ^? ceTypeFam & element & tfHead & dhNames
-- getClassElementDeclName (d @ ClsPatSig {}) = d ^? cePatSig & element & patSigName & semantics & nameInfo
-- getClassElementDeclName _ = []

-- getLocalDeclName :: LocalBind (NodeInfo (SemanticInfo n) src) -> Maybe n
-- getLocalDeclName (d @ LocalValBind {}) = listToMaybe (d ^? localVal & bindingName)
-- getLocalDeclName _ = Nothing

dhNames :: Simple Traversal (Ann DeclHead (NodeInfo (SemanticInfo n) src)) n
dhNames = declHeadNames & semantics & nameInfo


-- | A type class for transformations that work on both top-level and local definitions
class BindingElem d where
  sigBind :: Simple Partial (d a) (Ann TypeSignature a)
  valBind :: Simple Partial (d a) (Ann ValueBind a)
  createTypeSig :: Ann TypeSignature a -> d a
  createBinding :: Ann ValueBind a -> d a
  isTypeSig :: d a -> Bool
  isBinding :: d a -> Bool
  
instance BindingElem Decl where
  sigBind = declTypeSig
  valBind = declValBind
  createTypeSig = TypeSigDecl
  createBinding = ValueBinding
  isTypeSig (TypeSigDecl _) = True
  isTypeSig _ = False
  isBinding (ValueBinding _) = True
  isBinding _ = False

instance BindingElem LocalBind where
  sigBind = localSig
  valBind = localVal
  createTypeSig = LocalSignature
  createBinding = LocalValBind
  isTypeSig (LocalSignature _) = True
  isTypeSig _ = False
  isBinding (LocalValBind _) = True
  isBinding _ = False

bindName :: BindingElem d => Simple Traversal (d (NodeInfo (SemanticInfo n) src)) n
bindName = valBind&bindingName &+& sigBind&element&tsName&annList&semantics&nameInfo

valBindsInList :: BindingElem d => Simple Traversal (AnnList d a) (Ann ValueBind a)
valBindsInList = annList & element & valBind
     
getValBindInList :: (BindingElem d, HasRange a) => RealSrcSpan -> AnnList d a -> Maybe (Ann ValueBind a)
getValBindInList sp ls = case ls ^? valBindsInList & filtered (isInside sp) of
  [] -> Nothing
  [n] -> Just n
  _ -> error "getValBindInList: Multiple nodes"

nodesContaining :: forall node inner a . (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
                => RealSrcSpan -> Simple Traversal (node a) (inner a)
nodesContaining rng = biplateRef & filtered (isInside rng) 
              
isInside :: (HasAnnot node, HasRange a) => RealSrcSpan -> node a -> Bool
isInside rng nd = case getRange (getAnnot nd) of RealSrcSpan sp -> sp `containsSpan` rng
                                                 _ -> False
             
nodesWithRange :: forall node inner a . (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
               => RealSrcSpan -> Simple Traversal (node a) (inner a)
nodesWithRange rng = biplateRef & filtered (hasRange rng) 
                                         
hasRange :: (HasAnnot node, HasRange a) => RealSrcSpan -> node a -> Bool
hasRange rng node = case getRange (getAnnot node) of RealSrcSpan sp -> sp == rng
                                                     _ -> False

getNodeContaining :: (Biplate (node a) (Ann inner a), HasAnnot node, HasRange a) 
                  => RealSrcSpan -> node a -> Ann inner a
getNodeContaining sp node = case node ^? nodesContaining sp of
  [] -> error "getNodeContaining: The node cannot be found"
  results -> minimumBy (compareRangeLength `on` (getRange . (^. annotation))) results

compareRangeLength :: SrcSpan -> SrcSpan -> Ordering
compareRangeLength (RealSrcSpan sp1) (RealSrcSpan sp2)
  = (lineDiff sp1 `compare` lineDiff sp2) `mappend` (colDiff sp1 `compare` colDiff sp2)
  where lineDiff sp = srcLocLine (realSrcSpanStart sp) - srcLocLine (realSrcSpanEnd sp)
        colDiff sp = srcLocCol (realSrcSpanStart sp) - srcLocCol (realSrcSpanEnd sp)

getNode :: (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
        => RealSrcSpan -> node a -> inner a
getNode sp node = case node ^? nodesWithRange sp of
  [] -> error "getNode: The node cannot be found"
  [n] -> n
  _ -> error "getNode: Multiple nodes"
