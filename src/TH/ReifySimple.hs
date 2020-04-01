{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Utilities for reifying simplified datatype info. It omits details
-- that aren't usually relevant to generating instances that work with
-- the datatype. This makes it easier to use TH to derive instances.
--
-- The \"Simple\" in the module name refers to the simplicity of the
-- datatypes, not the module itself, which exports quite a few things
-- which are useful in some circumstance or another. I anticipate that
-- the most common uses of this will be the following APIs:
--
-- * Getting info about a @data@ or @newtype@ declaration, via
--   'DataType', 'reifyDataType', and 'DataCon'. This is useful for
--   writing something which generates declarations based on a datatype,
--   one of the most common uses of Template Haskell.
--
-- * Getting nicely structured info about a named type. See 'TypeInfo'
--   and 'reifyType'. This does not yet support reifying typeclasses,
--   primitive type constructors, or type variables ('TyVarI').
--
-- Currently, this module supports reifying simplified versions of the
-- following 'Info' constructors:
--
-- * 'TyConI' with 'DataD' and 'NewtypeD' (becomes a 'DataType' value)
--
-- * 'FamilyI' becomes a 'DataFamily' or 'TypeFamily' value.
--
-- * 'DataConI' becomes a 'DataCon' value.
--
-- In the future it will hopefully also have support for the remaining
-- 'Info' constructors, 'ClassI', 'ClassOpI', 'PrimTyConI', 'VarI', and
-- 'TyVarI'.
module TH.ReifySimple
    (
    -- * Reifying simplified type info
      TypeInfo, reifyType, infoToType
    , reifyTypeNoDataKinds, infoToTypeNoDataKinds
    -- * Reifying simplified info for specific declaration varieties
    -- ** Datatype info
    , DataType(..), reifyDataType, infoToDataType
    -- ** Data constructor info
    , DataCon(..), reifyDataCon, infoToDataCon, typeToDataCon
    -- ** Data family info
    , DataFamily(..), DataInst(..), reifyDataFamily, infoToDataFamily
    -- ** Type family info
    , TypeFamily(..), TypeInst(..), reifyTypeFamily, infoToTypeFamily
    -- * Other utilities
    , conToDataCons
    , reifyDataTypeSubstituted
    ) where

import           Control.Applicative
import           Data.Data (Data, gmapT)
import           Data.Generics.Aliases (extT)
import qualified Data.Map as M
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Language.Haskell.TH
#if MIN_VERSION_template_haskell(2,16,0)
                                     hiding (reifyType)
#endif
import           Language.Haskell.TH.Instances ()
import           TH.Utilities

data TypeInfo
    = DataTypeInfo DataType
    | DataFamilyInfo DataFamily
    | TypeFamilyInfo TypeFamily
    | LiftedDataConInfo DataCon

-- | Reifies a 'Name' as a 'TypeInfo', and calls 'fail' if this doesn't
-- work. Use 'reify' with 'infoToType' if you want to handle the failure
-- case more gracefully.
--
-- This does not yet support reifying typeclasses, primitive type
-- constructors, or type variables ('TyVarI').
reifyType :: Name -> Q TypeInfo
reifyType name = do
   info <- reify name
   mres <- infoToType info
   case mres of
       Just res -> return res
       Nothing -> fail $
           "Expected to reify a data type, data family, or type family. Instead got:\n" ++
           pprint info

-- | Convert an 'Info' into a 'TypeInfo' if possible, and otherwise
-- yield 'Nothing'.  Needs to run in 'Q' so that
infoToType :: Info -> Q (Maybe TypeInfo)
infoToType info =
    case (infoToTypeNoDataKinds info, infoToDataCon info) of
       (Just result, _) -> return (Just result)
       (Nothing, Just dc) -> do
#if MIN_VERSION_template_haskell(2,11,0)
           dataKindsEnabled <- isExtEnabled DataKinds
#else
           reportWarning $
               "For " ++ pprint (dcName dc) ++
               ", assuming DataKinds is on, and yielding LiftedDataConInfo."
           let dataKindsEnabled = True
#endif
           return $ if dataKindsEnabled then Just (LiftedDataConInfo dc) else Nothing
       (Nothing, Nothing) -> return Nothing

-- | Reifies type info, but instead of yielding a 'LiftedDataConInfo',
-- will instead yield 'Nothing'.
reifyTypeNoDataKinds :: Name -> Q (Maybe TypeInfo)
reifyTypeNoDataKinds = fmap infoToTypeNoDataKinds . reify

-- | Convert an 'Info into a 'TypeInfo' if possible. If it's a data
-- constructor, instead of yielding 'LiftedDataConInfo', it will instead
-- yield 'Nothing'.
infoToTypeNoDataKinds :: Info -> Maybe TypeInfo
infoToTypeNoDataKinds info =
   (DataTypeInfo <$> infoToDataType info) <|>
   (DataFamilyInfo <$> infoToDataFamily info) <|>
   (TypeFamilyInfo <$> infoToTypeFamily info)

--------------------------------------------------------------------------------
-- Reifying specific declaration varieties

-- | Simplified info about a 'DataD'. Omits deriving, strictness,
-- kind info, and whether it's @data@ or @newtype@.
data DataType = DataType
    { dtName :: Name
    , dtTvs :: [Name]
    , dtCxt :: Cxt
    , dtCons :: [DataCon]
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Simplified info about a 'Con'. Omits deriving, strictness, and kind
-- info. This is much nicer than consuming 'Con' directly, because it
-- unifies all the constructors into one.
data DataCon = DataCon
    { dcName :: Name
    , dcTvs :: [Name]
    , dcCxt :: Cxt
    , dcFields :: [(Maybe Name, Type)]
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Simplified info about a data family. Omits deriving, strictness, and
-- kind info.
data DataFamily = DataFamily
    { dfName :: Name
    , dfTvs :: [Name]
    , dfInsts :: [DataInst]
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Simplified info about a data family instance. Omits deriving,
-- strictness, and kind info.
data DataInst = DataInst
    { diName :: Name
    , diCxt :: Cxt
    , diParams :: [Type]
    , diCons :: [DataCon]
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Simplified info about a type family. Omits kind info and injectivity
-- info.
data TypeFamily = TypeFamily
    { tfName :: Name
    , tfTvs :: [Name]
    , tfInsts :: [TypeInst]
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Simplified info about a type family instance. Omits nothing.
data TypeInst = TypeInst
    { tiName :: Name
    , tiParams :: [Type]
    , tiType :: Type
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Reify the given data or newtype declaration, and yields its
-- 'DataType' representation.
reifyDataType :: Name -> Q DataType
reifyDataType name = do
    info <- reify name
    case infoToDataType info of
        Nothing -> fail $ "Expected to reify a datatype. Instead got:\n" ++ pprint info
        Just x -> return x

-- | Reify the given data constructor.
reifyDataCon :: Name -> Q DataCon
reifyDataCon name = do
    info <- reify name
    case infoToDataCon info of
        Nothing -> fail $ "Expected to reify a constructor. Instead got:\n" ++ pprint info
        Just x -> return x

-- | Reify the given data family, and yield its 'DataFamily'
-- representation.
reifyDataFamily :: Name -> Q DataFamily
reifyDataFamily name = do
    info <- reify name
    case infoToDataFamily info of
        Nothing -> fail $ "Expected to reify a data family. Instead got:\n" ++ pprint info
        Just x -> return x

-- | Reify the given type family instance declaration, and yields its
-- 'TypeInst' representation.
reifyTypeFamily :: Name -> Q TypeFamily
reifyTypeFamily name = do
    info <- reify name
    case infoToTypeFamily info of
        Nothing -> fail $ "Expected to reify a type family. Instead got:\n" ++ pprint info
        Just x -> return x

infoToDataType :: Info -> Maybe DataType
infoToDataType info = case info of
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (DataD preds name tvs _kind cons _deriving) ->
#else
    TyConI (DataD preds name tvs cons _deriving) ->
#endif
        Just $ DataType name (map tyVarBndrName tvs) preds (concatMap conToDataCons cons)
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (NewtypeD preds name tvs _kind con _deriving) ->
#else
    TyConI (NewtypeD preds name tvs con _deriving) ->
#endif
        Just $ DataType name (map tyVarBndrName tvs) preds (conToDataCons con)
    _ -> Nothing

infoToDataFamily :: Info -> Maybe DataFamily
infoToDataFamily info = case info of
#if MIN_VERSION_template_haskell(2,11,0)
    FamilyI (DataFamilyD name tvs _kind) insts ->
#else
    FamilyI (FamilyD DataFam name tvs _kind) insts ->
#endif
        Just $ DataFamily name (map tyVarBndrName tvs) (map go insts)
    _ -> Nothing
  where
#if MIN_VERSION_template_haskell(2,15,0)
    go (NewtypeInstD preds _ lhs _kind con _deriving)
      | ConT name:params <- unAppsT lhs
#elif MIN_VERSION_template_haskell(2,11,0)
    go (NewtypeInstD preds name params _kind con _deriving)
#else
    go (NewtypeInstD preds name params       con _deriving)
#endif
      = DataInst name preds params (conToDataCons con)
#if MIN_VERSION_template_haskell(2,15,0)
    go (DataInstD preds _ lhs _kind cons _deriving)
      | ConT name:params <- unAppsT lhs
#elif MIN_VERSION_template_haskell(2,11,0)
    go (DataInstD preds name params _kind cons _deriving)
#else
    go (DataInstD preds name params       cons _deriving)
#endif
      = DataInst name preds params (concatMap conToDataCons cons)
    go info' = error $
        "Unexpected instance in FamilyI in infoToDataInsts:\n" ++ pprint info'

infoToTypeFamily :: Info -> Maybe TypeFamily
infoToTypeFamily info = case info of
#if MIN_VERSION_template_haskell(2,11,0)
    FamilyI (ClosedTypeFamilyD (TypeFamilyHead name tvs _result _injectivity) eqns) _ ->
        Just $ TypeFamily name (map tyVarBndrName tvs) $ map (goEqn name) eqns
    FamilyI (OpenTypeFamilyD (TypeFamilyHead name tvs _result _injectivity)) insts ->
        Just $ TypeFamily name (map tyVarBndrName tvs) $ map (goInst name) insts
#else
    FamilyI (ClosedTypeFamilyD name tvs _kind eqns) [] ->
        Just $ TypeFamily name (map tyVarBndrName tvs) $ map (goEqn name) eqns
    FamilyI (FamilyD TypeFam name tvs _kind) insts ->
        Just $ TypeFamily name (map tyVarBndrName tvs) $ map (goInst name) insts
#endif
    _ -> Nothing
  where
#if MIN_VERSION_template_haskell(2,15,0)
    toParams ps (AppT ty p) = toParams (p : ps) ty
    toParams ps (AppKindT ty _) = toParams ps ty
    toParams ps _ = ps
    goEqn name (TySynEqn _ lty rty) = TypeInst name (toParams [] lty) rty
    goInst name (TySynInstD eqn) = goEqn name eqn
    goInst _ info' = error $
        "Unexpected instance in FamilyI in infoToTypeInsts:\n" ++ pprint info'
#else
    goEqn name (TySynEqn params ty) = TypeInst name params ty
    goInst name (TySynInstD _ eqn) = goEqn name eqn
    goInst _ info' = error $
        "Unexpected instance in FamilyI in infoToTypeInsts:\n" ++ pprint info'
#endif

infoToDataCon :: Info -> Maybe DataCon
infoToDataCon info = case info of
#if MIN_VERSION_template_haskell(2,11,0)
    DataConI name ty _parent ->
#else
    DataConI name ty _parent _fixity ->
#endif
        Just (typeToDataCon name ty)
    _ -> Nothing

-- | Creates a 'DataCon' given the 'Name' and 'Type' of a
-- data-constructor.  Note that the result the function type is *not* checked to match the provided 'Name'.
typeToDataCon :: Name -> Type -> DataCon
typeToDataCon dcName ty0 = DataCon {..}
  where
    (dcTvs, dcCxt, dcFields) = case ty0 of
        ForallT tvs preds ty -> (map tyVarBndrName tvs, preds, typeToFields ty)
        ty -> ([], [], typeToFields ty)
    -- TODO: Should we sanity check the result type?
    typeToFields = init . map (Nothing, ) . unAppsT

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
        map (\dn -> DataCon dn [] [] (map (\(_, ty) -> (Nothing, ty)) slots)) ns
    RecGadtC ns fields _ ->
        map (\dn -> DataCon dn [] [] (map (\(fn, _, ty) -> (Just fn, ty)) fields)) ns
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
