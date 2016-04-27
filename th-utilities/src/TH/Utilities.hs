{-# LANGUAGE TemplateHaskell #-}

-- | Miscellaneous Template Haskell utilities, added as needed by
-- packages in the th-utilities repo and elsewhere.
module TH.Utilities where

import Data.Proxy
import Language.Haskell.TH

-- | Get the 'Name' of a 'TyVarBndr'
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n k) = n

-- | Breaks a type application like @A b c@ into [A, b, c].
unAppsT :: Type -> [Type]
unAppsT = go []
  where
    go xs (AppT l x) = go (x : xs) l
    go xs ty = ty : xs

-- | Given a type, construct the expression (Proxy :: Proxy ty).
proxyE :: TypeQ -> ExpQ
proxyE ty = [| Proxy :: Proxy $(ty) |]

-- | Construct a plain name ('mkName') based on the given name. This is
-- useful for cases where TH doesn't expect a unique name.
dequalify :: Name -> Name
dequalify = mkName . nameBase
