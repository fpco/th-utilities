{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Miscellaneous Template Haskell utilities, added as needed by
-- packages in the th-utilities repo and elsewhere.
module TH.Utilities where

import Data.Proxy
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Get the 'Name' of a 'TyVarBndr'
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n _) = n

-- | Breaks a type application like @A b c@ into [A, b, c]. In other
-- words, it descends leftwards down 'AppT' constructors, and yields a
-- list of the results.
unAppsT :: Type -> [Type]
unAppsT = go []
  where
    go xs (AppT l x) = go (x : xs) l
    go xs ty = ty : xs

-- | Given a 'Type', returns a 'Just' value if it's a named type
-- constructor applied to arguments. This value contains the name of the
-- type and a list of arguments.
typeToNamedCon :: Type -> Maybe (Name, [Type])
#if MIN_VERSION_template_haskell(2,11,0)
typeToNamedCon (InfixT l n r) = Just (n, [l, r])
typeToNamedCon (UInfixT l n r) = Just (n, [l, r])
#endif
typeToNamedCon (unAppsT -> (ConT n : args)) = Just (n, args)
typeToNamedCon _ = Nothing

-- | Expect the provided type to be an application of a regular type to
-- one argument, otherwise fail with a message. This will also work if
-- the name is a promoted data constructor ('PromotedT').
expectTyCon1 :: Name -> Type -> Q Type
expectTyCon1 expected (AppT (ConT n) x) | expected == n = return x
expectTyCon1 expected (AppT (PromotedT n) x) | expected == n = return x
expectTyCon1 expected x = fail $
    "Expected " ++ pprint expected ++
    ", applied to one argument, but instead got " ++ pprint x ++ "."

-- | Expect the provided type to be an application of a regular type to
-- two arguments, otherwise fail with a message. This will also work if
-- the name is a promoted data constructor ('PromotedT').
expectTyCon2 :: Name -> Type -> Q (Type, Type)
expectTyCon2 expected (AppT (AppT (ConT n) x) y) | expected == n = return (x, y)
expectTyCon2 expected (AppT (AppT (PromotedT n) x) y) | expected == n = return (x, y)
#if MIN_VERSION_template_haskell(2,11,0)
expectTyCon2 expected (InfixT x n r) | expected == n = return (x, y)
expectTyCon2 expected (UInfixT x n r) | expected == n = return (x, y)
#endif
expectTyCon2 expected x = fail $
    "Expected " ++ pprint expected ++
    ", applied to two arguments, but instead got " ++ pprint x ++ "."

-- | Given a type, construct the expression (Proxy :: Proxy ty).
proxyE :: TypeQ -> ExpQ
proxyE ty = [| Proxy :: Proxy $(ty) |]

-- | Construct a plain name ('mkName') based on the given name. This is
-- useful for cases where TH doesn't expect a unique name.
dequalify :: Name -> Name
dequalify = mkName . nameBase

-- | Get the free type variables of a 'Type'.
freeVarsT :: Type -> [Name]
freeVarsT (ForallT tvs _ ty) = filter (`notElem` (map tyVarBndrName tvs)) (freeVarsT ty)
freeVarsT (AppT l r) = freeVarsT l ++ freeVarsT r
freeVarsT (SigT ty k) = freeVarsT ty ++ freeVarsT k
freeVarsT (VarT n) = [n]
#if MIN_VERSION_template_haskell(2,11,0)
freeVarsT (InfixT x n r) = freeVarsT x ++ freeVarsT y
freeVarsT (UInfixT x n r) = freeVarsT x ++ freeVarsT y
#endif
freeVarsT _ = []

-- | Hack to enable putting expressions inside 'lift'-ed TH data. For
-- example, you could do
--
-- @
--     main = print $(lift [ExpLifter [e| 1 + 1 |],  ExpLifter [e| 2 |]])
-- @
--
-- Without 'ExpLifter', 'lift' tends to just generate code involving
-- data construction. With 'ExpLifter', you can put more complicated
-- expression into this construction.
data ExpLifter = ExpLifter ExpQ deriving (Typeable)

instance Lift ExpLifter where
  lift (ExpLifter e) = e
