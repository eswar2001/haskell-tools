{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Tools.BackendGHC.TH where

import GHC.Hs.Expr as GHC (HsSplice, HsBracket)
import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..))
import GHC.Hs.Extension (GhcPass)

trfQuasiQuotation' :: TransformName n r => HsSplice n -> Trf (AST.UQuasiQuote (Dom r) RangeStage)
trfSplice :: (TransformName n r, n ~ GhcPass p) => HsSplice n -> Trf (Ann AST.USplice (Dom r) RangeStage)
trfSplice' :: (TransformName n r, n ~ GhcPass p) => HsSplice n -> Trf (AST.USplice (Dom r) RangeStage)
trfBracket' :: (TransformName n r, n ~ GhcPass p) => HsBracket n -> Trf (AST.UBracket (Dom r) RangeStage)
