{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Miscellaneous Template Haskell utilities, added as needed by
-- packages in the th-utilities repo and elsewhere.
module TH.Utilities where

import Control.Monad (foldM)
import Data.Data
import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndr_, tvName)
import Text.Read (readMaybe)
import TH.FixQ (fixQ)

-- | Get the 'Name' of a 'TyVarBndr'
tyVarBndrName :: TyVarBndr_ flag -> Name
tyVarBndrName = tvName

appsT :: Type -> [Type] -> Type
appsT x [] = x
appsT x (y:xs) = appsT (AppT x y) xs

-- | Breaks a type application like @A b c@ into [A, b, c]. In other
-- words, it descends leftwards down 'AppT' constructors, and yields a
-- list of the results.
unAppsT :: Type -> [Type]
unAppsT = go []
  where
    go xs (AppT l x) = go (x : xs) l
    go xs ty = ty : xs

-- | Given a list of types, produce the type of a tuple of
-- those types. This is analogous to 'tupE' and 'tupP'.
--
-- @
-- tupT [[t|Int|], [t|Char|], [t|Bool]] = [t| (Int, Char, Bool) |]
-- @
--
-- @since FIXME
tupT :: [Q Type] -> Q Type
tupT ts = do
  -- We build the expression with a thunk inside that will be filled in with
  -- the length of the list once that's been determined. This works
  -- efficiently (in one pass) because TH.Type is rather lazy.
  (res, !_n) <- fixQ (\ ~(_res, n) -> foldM go (TupleT n, 0) ts)
  pure res
  where
    go (acc, !k) ty = do
      ty' <- ty
      pure (acc `AppT` ty', k + 1)

-- | Given a list of types, produce the type of a promoted tuple of
-- those types. This is analogous to 'tupE' and 'tupP'.
--
-- @
-- promotedTupT [[t|3|], [t| 'True|], [t|Bool]] = [t| '(3, 'True, Bool) |]
-- @
--
-- @since FIXME
promotedTupT :: [Q Type] -> Q Type
promotedTupT ts = do
  -- We build the expression with a thunk inside that will be filled in with
  -- the length of the list once that's been determined. This works
  -- efficiently (in one pass) because TH.Type is rather lazy.
  (res, !_n) <- fixQ (\ ~(_res, n) -> foldM go (PromotedTupleT n, 0) ts)
  pure res
  where
    go (acc, !k) ty = do
      ty' <- ty
      pure (acc `AppT` ty', k + 1)

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
expectTyCon2 expected (InfixT x n y) | expected == n = return (x, y)
expectTyCon2 expected (UInfixT x n y) | expected == n = return (x, y)
#endif
expectTyCon2 expected x = fail $
    "Expected " ++ pprint expected ++
    ", applied to two arguments, but instead got " ++ pprint x ++ "."

-- | Given a type, construct the expression (Proxy :: Proxy ty).
proxyE :: TypeQ -> ExpQ
proxyE ty = [| Proxy :: Proxy $(ty) |]

-- | Like the 'everywhere' generic traversal strategy, but skips over
-- strings. This can aid performance of TH traversals quite a bit.
everywhereButStrings :: Data a => (forall b. Data b => b -> b) -> a -> a
everywhereButStrings f =
    (f . gmapT (everywhereButStrings f)) `extT` (id :: String -> String)

-- | Like the 'everywhereM' generic traversal strategy, but skips over
-- strings. This can aid performance of TH traversals quite a bit.
everywhereButStringsM :: forall a m. (Data a, Monad m) => GenericM m -> a -> m a
everywhereButStringsM f x = do
    x' <- gmapM (everywhereButStringsM f) x
    (f `extM` (return :: String -> m String)) x'

-- | Make a 'Name' with a 'NameS' or 'NameQ' flavour, from a 'Name' with
-- any 'NameFlavour'. This may change the meaning of names.
toSimpleName :: Name -> Name
toSimpleName = mkName . pprint

-- | Construct a plain name ('mkName') based on the given name. This is
-- useful for cases where TH doesn't expect a unique name.
dequalify :: Name -> Name
dequalify = mkName . nameBase

-- | Apply 'dequalify' to every type variable.
dequalifyTyVars :: Data a => a -> a
dequalifyTyVars = everywhere (id `extT` modifyType)
  where
    modifyType (VarT n) = VarT (dequalify n)
    modifyType ty = ty

-- | Get the free type variables of a 'Type'.
freeVarsT :: Type -> [Name]
freeVarsT (ForallT tvs _ ty) = filter (`notElem` (map tyVarBndrName tvs)) (freeVarsT ty)
freeVarsT (VarT n) = [n]
freeVarsT ty = concat $ gmapQ (const [] `extQ` freeVarsT) ty

-- | Utility to conveniently handle change to 'InstanceD' API in
-- template-haskell-2.11.0
plainInstanceD :: Cxt -> Type -> [Dec] -> Dec
plainInstanceD =
#if MIN_VERSION_template_haskell(2,11,0)
    InstanceD Nothing
#else
    InstanceD
#endif

-- | Utility to conveniently handle change to 'InstanceD' API in
-- template-haskell-2.11.0
fromPlainInstanceD :: Dec -> Maybe (Cxt, Type, [Dec])
#if MIN_VERSION_template_haskell(2,11,0)
fromPlainInstanceD (InstanceD _ a b c) = Just (a, b, c)
#else
fromPlainInstanceD (InstanceD a b c) = Just (a, b, c)
#endif
fromPlainInstanceD _ = Nothing

-- | Utility to convert "Data.Typeable" 'TypeRep' to a 'Type'. Note that
-- this function is known to not yet work for many cases, but it does
-- work for normal user datatypes. In future versions this function
-- might have better behavior.
typeRepToType :: TypeRep -> Q Type
typeRepToType tr = do
    let (con, args) = splitTyConApp tr
        modName = tyConModule con
        name pn mn cn = Name (OccName cn) (NameG TcClsName (PkgName pn) (ModName mn))
        conName = tyConName con
        t | modName == tupleMod = TupleT $ length args
          | modName == listMod && conName == listCon = ListT
          | modName == typeLitsMod =
              case tyConName con of
                s@('"':_) -> LitT . StrTyLit $ read s
#if MIN_VERSION_template_haskell(2,18,0)
                ['\'', c, '\''] -> LitT $ CharTyLit c
#endif
                s -> case readMaybe s of
                    Just n -> LitT $ NumTyLit n
                    _ -> error $ "Unrecognized type literal name: " ++ s
          | otherwise = ConT $ name (tyConPackage con) modName conName
    resultArgs <- mapM typeRepToType args
    return (appsT t resultArgs)
  where
    typeLitsMod = tyConModule . typeRepTyCon . typeRep $ Proxy @42
    tupleMod = tyConModule . typeRepTyCon . typeRep $ Proxy @((), ())
    listMod = tyConModule . typeRepTyCon . typeRep $ Proxy @[()]
    listCon = tyConName . typeRepTyCon . typeRep $ Proxy @[()]


-- | Hack to enable putting expressions inside 'lift'-ed TH data. For
-- example, you could do
--
-- @
--     main = print $(lift [ExpLifter [e| 1 + 1 |],  ExpLifter [e| 2 |]])
-- @
--
-- Here, 'lift' is working on a value of type @[ExpLifter]@. The code
-- generated by 'lift' constructs a list with the 'ExpLifter'
-- expressions providing the element values.
--
-- Without 'ExpLifter', 'lift' tends to just generate code involving
-- data construction. With 'ExpLifter', you can put more complicated
-- expression into this construction.
--
-- Note that this cannot be used in typed quotes, because 'liftTyped'
-- will throw an exception. This is because this hack is incompatible
-- with the type of 'liftTyped', as it would require the generated
-- code to have type 'ExpLifter'.
data ExpLifter = ExpLifter
#if __GLASGOW_HASKELL__ >= 811
  (forall m. Quote m => m Exp)
#else
  ExpQ
#endif
  deriving (Typeable)

instance Lift ExpLifter where
  lift (ExpLifter e) = e
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = error $ concat
    [ "'liftTyped' is not implemented for 'ExpLifter', "
    , "because it would require the generated code to have type 'ExpLifter'"
    ]
#endif

-- | Print splices generated by a TH splice (the printing will happen
-- during compilation, as a GHC warning). Useful for debugging.
--
-- For instance, you can dump splices generated with 'makeLenses' by
-- replacing a top-level invocation of 'makeLenses' in your code with:
--
-- @dumpSplices $ makeLenses ''Foo@
dumpSplices :: DecsQ -> DecsQ
dumpSplices x = do
  ds <- x
  let code = lines (pprint ds)
  reportWarning ("\n" ++ unlines (map ("    " ++) code))
  return ds
