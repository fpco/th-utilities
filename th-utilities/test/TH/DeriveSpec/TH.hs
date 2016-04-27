{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TH.DeriveSpec.TH where

import TH.Derive
import Language.Haskell.TH
import Data.Proxy

class InstShowBlind a

instance Instantiator (InstShowBlind a) where
    runInstantiator _ preds (AppT (ConT ((== ''InstShowBlind) -> True)) ty)  []= do
        expr <- [| \_ -> "ShowBlind" |]
        return [InstanceD preds (AppT (ConT ''Show) ty) [ValD (VarP 'show) (NormalB expr) []]]
    runInstantiator _ _ _ _ =
        fail "Theoretically impossible case in InstShowBlind instantiator for Show"

class InstShowConst a where constResult :: Proxy a -> String

instance Instantiator (InstShowConst a) where
    runInstantiator _ preds (AppT (ConT ((== ''InstShowConst) -> True)) ty) decls =
        dequalifyMethods ''InstShowConst =<<
        sequence
        [ instanceD (return preds) [t| Show $(return ty) |] $
            [valD (varP 'show)
                  (normalB [| \_ -> constResult undefined |])
                  (map return decls)]]
    runInstantiator _ _ _ _ =
        fail "Theoretically impossible case in InstShowConst instantiator for Show"

class Eq b => InstEqBy a b where
    toEq :: a -> b

instance Instantiator (InstEqBy a b) where
    runInstantiator _ preds (AppT (AppT (ConT ((== ''InstEqBy) -> True)) aTy) bTy) decls =
        dequalifyMethods ''InstEqBy =<<
        sequence
        [ instanceD (return preds) [t| Eq $(return aTy) |] $
            [valD (varP '(==))
                  (normalB [| \l r -> toEq l == toEq r |])
                  (map return decls)]
        ]
