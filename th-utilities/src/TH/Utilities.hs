{-# LANGUAGE TemplateHaskell #-}

module TH.Utilities where

import Data.Proxy
import Language.Haskell.TH

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n k) = n

-- | Breaks a type application like @A b c@ into [A, b, c].
unAppsT :: Type -> [Type]
unAppsT = go []
  where
    go xs (AppT l x) = go (x : xs) l
    go xs ty = ty : xs

proxyE :: TypeQ -> ExpQ
proxyE ty = [| Proxy :: Proxy $(ty) |]

dequalify :: Name -> Name
dequalify = mkName . nameBase
