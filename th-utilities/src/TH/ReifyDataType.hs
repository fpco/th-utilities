{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | Utilities for reifying simplified datatype info. It omits details
-- that aren't usually relevant to generating instances that work with
-- the datatype. This makes it easier to use TH to derive instances.
module TH.ReifyDataType
    ( DataType(..)
    , DataCon(..)
    , reifyDataType
    , conToDataCons
    , reifyDataTypeSubstituted
    ) where

import           Data.Data (Data, gmapT)
import           Data.Generics.Aliases (extT)
import qualified Data.Map as M
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Language.Haskell.TH
import           TH.Utilities

-- | Simplified info about a 'DataD'. Omits deriving, strictness, and
-- kind info.
data DataType = DataType
    { dtName :: Name
    , dtTvs :: [Name]
    , dtCxt :: Cxt
    , dtCons :: [DataCon]
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Simplified info about a 'Con'. Omits strictness, and kind info.
-- This is much nicer than consuming 'Con' directly, because it unifies
-- all the constructors into one.
data DataCon = DataCon
    { dcName :: Name
    , dcTvs :: [Name]
    , dcCxt :: Cxt
    , dcFields :: [(Maybe Name, Type)]
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Reify the given data or newtype declaration, and yields its
-- 'DataType' representation.
reifyDataType :: Name -> Q DataType
reifyDataType queryName = do
    info <- reify queryName
    case info of
#if MIN_VERSION_template_haskell(2,11,0)
        TyConI (DataD cxt name vars _kind cons _deriving) -> do
#else
        TyConI (DataD cxt name vars cons _deriving) -> do
#endif
            let tvs = map tyVarBndrName vars
                cs = concatMap conToDataCons cons
            return (DataType name tvs cxt cs)
#if MIN_VERSION_template_haskell(2,11,0)
        TyConI (NewtypeD cxt name vars _kind con _deriving) -> do
#else
        TyConI (NewtypeD cxt name vars con _deriving) -> do
#endif
            let tvs = map tyVarBndrName vars
                cs = conToDataCons con
            return (DataType name tvs cxt cs)
        _ -> fail $ "Expected to reify a datatype, instead got:\n" ++ pprint info

-- | Convert a 'Con' to a list of 'DataCon'. The result is a list
-- because 'GadtC' and 'RecGadtC' can define multiple constructors.
conToDataCons :: Con -> [DataCon]
conToDataCons = \case
    NormalC name slots ->
        [DataCon name [] [] (map (\(_, ty) -> (Nothing, ty)) slots)]
    RecC name fields ->
        [DataCon name [] [] (map (\(n, _, ty) -> (Just n, ty)) fields)]
    InfixC (_, ty1) name (_, ty2) ->
        [DataCon name [] [] [(Nothing, ty1), (Nothing, ty2)]]
    ForallC tvs preds con ->
        map (\(DataCon name tvs0 preds0 fields) ->
            DataCon name (tvs0 ++ map tyVarBndrName tvs) (preds0 ++ preds) fields) (conToDataCons con)
#if MIN_VERSION_template_haskell(2,11,0)
    GadtC ns slots _ ->
        map (\n -> DataCon n [] [] (map (\(_, ty) -> (Nothing, ty)) slots)) ns
    RecGadtC ns fields _ ->
        map (\n -> DataCon n [] [] (map (\(n, _, ty) -> (Just n, ty)) fields)) ns
#endif

-- | Like 'reifyDataType', but takes a 'Type' instead of just the 'Name'
-- of the datatype. It expects a normal datatype argument (see
-- 'typeToNamedCon').
reifyDataTypeSubstituted :: Type -> Q DataType
reifyDataTypeSubstituted ty =
    case typeToNamedCon ty of
        Nothing -> fail $ "Expected a datatype, but reifyDataTypeSubstituted was applied to " ++ pprint ty
        Just (n, args) -> do
            dt <- reifyDataType n
            let cons' = substituteTvs (M.fromList (zip (dtTvs dt) args)) (dtCons dt)
            return (dt { dtCons = cons' })

-- TODO: add various handy generics based traversals to TH.Utilities

substituteTvs :: Data a => M.Map Name Type -> a -> a
substituteTvs mp = transformTypes go
  where
    go (VarT name) | Just ty <- M.lookup name mp = ty
    go ty = gmapT (substituteTvs mp) ty

transformTypes :: Data a => (Type -> Type) -> a -> a
transformTypes f = gmapT (transformTypes f) `extT` (id :: String -> String) `extT` f
