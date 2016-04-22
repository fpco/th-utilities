{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module TH.ReifyDataType where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Haskell.TH
import TH.Utilities

-- | Simplified info about a 'DataD'. Omits deriving, strictness, and
-- kind info.
data DataType = DataType Name [Name] Cxt [DataCon]
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Simplified info about a 'Con'. Omits strictness, and kind info.
-- This is much nicer than consuming 'Con' directly, because it unifies
-- all the constructors into one.
data DataCon = DataCon Name [Name] Cxt [(Maybe Name, Type)]
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

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
