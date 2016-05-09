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

class InstShowConst a where _constResult :: Proxy a -> String

instance Instantiator (InstShowConst a) where
    runInstantiator _ preds (AppT (ConT ((== ''InstShowConst) -> True)) ty) decls =
        dequalifyMethods ''InstShowConst =<<
        sequence
        [ instanceD (return preds) [t| Show $(return ty) |] $
            [valD (varP 'show)
                  (normalB [| \_ -> _constResult undefined |])
                  (map return decls)]]
    runInstantiator _ _ _ _ =
        fail "Theoretically impossible case in InstShowConst instantiator for Show"

class Ord o => InstEqOrdVia o a where
    _toOrd :: a -> o

instance Instantiator (InstEqOrdVia o a) where
    runInstantiator _ preds (AppT (AppT (ConT ((== ''InstEqOrdVia) -> True)) _oTy) aTy) decls =
        dequalifyMethods ''InstEqOrdVia =<<
        sequence
        [instanceD (return preds) [t| Eq $(return aTy) |] $
            [valD (varP '(==))
                  (normalB [| \l r -> _toOrd l == _toOrd r |])
                  (map return decls)]

        , instanceD (return preds) [t| Ord $(return aTy) |] $
            [valD (varP 'compare)
                  (normalB [| \l r -> compare (_toOrd l) (_toOrd r) |])
                  (map return decls)
            ]
        ]
    runInstantiator _ _ _ _ =
        fail "Theoretically impossible case in InstEqOrdVia instantiator"
