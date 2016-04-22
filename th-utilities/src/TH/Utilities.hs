module TH.Utilities where

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
