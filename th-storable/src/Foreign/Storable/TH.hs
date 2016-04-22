{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Foreign.Storable.TH
    ( derive
    ) where

import           Control.Monad
import           Data.Data (Data, gmapT)
import           Data.Generics.Aliases (extT)
import           Data.List (find)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Primitive.Types (Prim(..))
import           Data.Typeable
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

--TODO: support constraint kinds, for concision!

derive :: Q [Dec] -> Q [Dec]
derive decs = return . concat =<< mapM deriveInstance =<< decs

deriveInstance :: Dec -> Q [Dec]
deriveInstance (InstanceD cxt ty@(unAppsT -> ((ConT className):args)) []) =
    if className == ''Storable
       then case args of
           [unAppsT -> (ConT name:args')] -> do
               DataType tvs cons <- reifyDataTypeOrFail (TypeConstructor name)
               let cons' = substituteTvs (M.fromList (zip tvs args')) cons
               makeStorable cxt ty cons'
           _ -> fail "Expected concrete datatype to derive for Storable instance"
       else fail $ "derive doesn't know how to derive instances of " ++ show className
deriveInstance _ = fail "derive only expects empty instance declarations"

substituteTvs :: Data a => M.Map TypeVariable Type -> a -> a
substituteTvs mp = transformTypes go
  where
    go (VarT name) | Just ty <- M.lookup (TypeVariable name) mp = ty
    go ty = gmapT (substituteTvs mp) ty

-- TODO: recursion check? At least document that this could in some
-- cases work, but produce a bogus instance.

makeStorable :: Cxt -> Type -> [Constructor] -> Q [Dec]
makeStorable cxt headTy cons = do
    -- Since this instance doesn't pay attention to alignment, we
    -- just say alignment doesn't matter.
    alignmentMethod <- [| 1 |]
    sizeOfMethod <- sizeExpr
    peekMethod <- peekExpr
    pokeMethod <- pokeExpr
    let methods =
            [ FunD (mkName "alignment") [Clause [WildP] (NormalB alignmentMethod) []]
            , FunD (mkName "sizeOf") [Clause [WildP] (NormalB sizeOfMethod) []]
            , FunD (mkName "peek") [Clause [VarP ptrName] (NormalB peekMethod) []]
            , FunD (mkName "poke") [Clause [VarP ptrName, VarP valName] (NormalB pokeMethod) []]
            ]
    return [InstanceD cxt headTy methods]
  where
    (tagType, _, tagSize) =
        fromMaybe (error "Too many constructors") $
        find (\(_, maxN, _) -> maxN >= length cons) tagTypes
    tagTypes :: [(Name, Int, Int)]
    tagTypes =
        [ ('(), 1, 0)
        , (''Word8, fromIntegral (maxBound :: Word8), 1)
        , (''Word16, fromIntegral (maxBound :: Word16), 2)
        , (''Word32, fromIntegral (maxBound :: Word32), 4)
        , (''Word64, fromIntegral (maxBound :: Word64), 8)
        ]
    valName = mkName "val"
    tagName = mkName "tag"
    ptrName = mkName "ptr"
    fName ix = mkName ("f" ++ show ix)
    ptrExpr = varE ptrName
    -- [[Int]] expression, where the inner lists are the sizes of the
    -- fields. Each member of the outer list corresponds to a different
    -- constructor.
    sizeExpr = appE (varE 'maximum) $
        listE [ appE (varE 'sum) (listE [sizeOfExpr ty | (_, ty) <- fields])
              | (Constructor (ValueConstructor cname) fields) <- cons
              ]
    -- Choose a tag size large enough for this constructor count.
    -- Expression used for the definition of peek.
    peekExpr = case cons of
        [] -> [| error ("Attempting to peek type with no constructors (" ++ $(lift (show headTy)) ++ ")") |]
        [constr] -> peekConstr constr
        _ -> doE
            [ bindS (varP tagName) [| peek (castPtr $(ptrExpr)) |]
            , noBindS (caseE (sigE (varE tagName) (conT tagType)) (map peekMatch (zip [0..] cons)))
            ]
    peekMatch (ix, con) = match (litP (IntegerL ix)) (normalB (peekConstr con)) []
    peekConstr (Constructor (ValueConstructor cname) fields) =
        letE (offsetDecls fields) $
        case fields of
            [] -> [| pure $(conE cname) |]
            (_:fields') ->
                foldl (\acc (ix, _) -> [| $(acc) <*> $(peekOffset ix) |] )
                      [| $(conE cname) <$> $(peekOffset 0) |]
                      (zip [1..] fields')
    peekOffset ix = [| peek (castPtr (plusPtr $(ptrExpr) $(varE (offset ix)))) |]
    -- Expression used for the definition of poke.
    pokeExpr = caseE (varE valName) (map pokeMatch (zip [0..] cons))
    pokeMatch :: (Int, Constructor) -> MatchQ
    pokeMatch (ixcon, Constructor (ValueConstructor cname) fields) =
        match (conP cname (map varP (map fName ixs)))
              (normalB (doE (tagPokes ++ offsetLet ++ fieldPokes)))
              []
      where
        tagPokes = case cons of
            (_:_:_) -> [noBindS [| poke (castPtr $(ptrExpr)) (ixcon :: $(conT tagType)) |]]
            _ -> []
        offsetLet
            | null ixs = []
            | otherwise = [letS (offsetDecls fields)]
        fieldPokes = map (noBindS . pokeField) ixs
        ixs = map fst (zip [0..] fields)
    pokeField ix = [| poke (castPtr (plusPtr $(ptrExpr)
                                             $(varE (offset ix))))
                           $(varE (fName ix)) |]
    -- Generate declarations which compute the field offsets.
    offsetDecls fields =
        -- Skip the last one, to avoid unused variable warnings.
        init $
        map (\(ix, expr) -> valD (varP (offset ix)) (normalB expr) []) $
        -- Initial offset is the tag size.
        ((0, [| tagSize |]) :) $
        map (\(ix, (_, ty)) -> (ix, offsetExpr ix ty)) $
        zip [1..] fields
      where
        offsetExpr ix ty = [| $(sizeOfExpr ty) + $(varE (offset (ix - 1))) |]
    sizeOfExpr ty = [| $(varE 'sizeOf) (error "sizeOf evaluated its argument" :: $(return ty)) |]
    offset ix = mkName ("offset" ++ show ix)

-- Misc util

transformTypes :: Data a => (Type -> Type) -> a -> a
transformTypes f = gmapT (transformTypes f) `extT` (id :: String -> String) `extT` f

-- | Breaks a type application like @A b c@ into [A, b, c].
unAppsT :: Type -> [Type]
unAppsT = go []
  where
    go xs (AppT l x) = go (x : xs) l
    go xs ty = ty : xs

reifyDataTypeOrFail :: TypeConstructor -> Q DataType
reifyDataTypeOrFail tycon@(TypeConstructor name) = do
    mtypeDefinition <- reifyTypeDefinition tycon
    case mtypeDefinition of
      Nothing -> fail $ "No datatype named " ++ show name
      Just typeDefinition ->
          case typeDefinition of
            TypeAliasDefinition{} ->
                fail $ "Expected datatype named " ++ show name ++ ", but got found a type synonym"
            DataTypeDefinition _ dt ->
                return dt

-- The is copy-modified from Chris Done's present package
-- https://github.com/chrisdone/present/blob/b0a8d7e0f4f4fbd297869ee49574de5a3c6f576b/src/Present.hs#L258

-- TODO: Extract this stuff into a package, or have present export them.

--------------------------------------------------------------------------------
-- Type Reification
--
-- We have to reify all the type constructors involved in a given
-- type.
--

-- | Name of a variable.
newtype ValueVariable =
  ValueVariable Name
  deriving (Eq, Show, Ord, Data, Typeable)

-- | Name of a value constructor.
newtype ValueConstructor =
  ValueConstructor Name
  deriving (Eq, Show, Ord, Data, Typeable)

-- | A normalize representation of a constructor. Present's main
-- algorithm doesn't particularly care whether it's infix, a record,
-- or whatever.
data Constructor =
  Constructor {_constructorName :: ValueConstructor
              ,constructorFields :: [(Maybe ValueVariable,Type)]}
  deriving (Eq, Show, Ord, Data, Typeable)

-- | A data type.
data DataType =
  DataType {_dataTypeVariables :: [TypeVariable]
           ,_dataTypeConstructors :: [Constructor]}
  deriving (Eq, Show, Ord, Data, Typeable)

-- | A type alias.
data TypeAlias =
  TypeAlias {_aliasVariables :: [TypeVariable]
            ,_aliasType :: Type}
  deriving (Eq, Show, Ord, Data, Typeable)

-- | Definition of a type.
data TypeDefinition
  = DataTypeDefinition TypeConstructor
                       DataType
  | TypeAliasDefinition TypeConstructor
                        TypeAlias
  deriving (Eq, Show, Ord, Data, Typeable)

-- | A type variable.
newtype TypeVariable =
  TypeVariable Name
  deriving (Eq, Show, Ord, Data, Typeable)

-- | A type constructor.
newtype TypeConstructor =
  TypeConstructor Name
  deriving (Eq, Show, Ord, Data, Typeable)

-- | Reify all the constructors of a name. Unless it's primitive, in
-- which case return nothing.
reifyTypeDefinition
  :: TypeConstructor -> Q (Maybe TypeDefinition)
reifyTypeDefinition typeConstructor@(TypeConstructor name) =
  do info <- reify name
     let result =
           case info of
             TyConI dec ->
               case dec of
                 DataD _cxt _ vars cons _deriving ->
                   do cs <- mapM makeConstructor cons
                      return (Just (DataTypeDefinition typeConstructor
                                                       (DataType (map toTypeVariable vars) cs)))
                 NewtypeD _cxt _ vars con _deriving ->
                   do c <- makeConstructor con
                      return (Just (DataTypeDefinition
                                      typeConstructor
                                      (DataType (map toTypeVariable vars)
                                                [c])))
                 TySynD _ vars ty ->
                   return (Just (TypeAliasDefinition typeConstructor
                                                     (TypeAlias (map toTypeVariable vars) ty)))
                 _ -> fail "Not a supported data type declaration."
             PrimTyConI{} -> return Nothing
             FamilyI{} -> fail "Data families not supported yet."
             _ ->
               fail ("Not a supported object, no type inside it: " ++
                     pprint info)
     case result of
       Left err -> fail err
       Right ok -> return ok

-- | Convert a TH type variable to a normalized type variable.
toTypeVariable :: TyVarBndr -> TypeVariable
toTypeVariable =
  \case
    PlainTV t -> TypeVariable t
    KindedTV t _ -> TypeVariable t

-- | Make a normalized constructor from the more complex TH Con.
makeConstructor
  :: Con -> Either String Constructor
makeConstructor =
  \case
    NormalC name slots ->
      Constructor <$> pure (ValueConstructor name) <*> mapM makeSlot slots
    RecC name fields ->
      Constructor <$> pure (ValueConstructor name) <*> mapM makeField fields
    InfixC t1 name t2 ->
      Constructor <$> pure (ValueConstructor name) <*>
      ((\x y -> [x,y]) <$> makeSlot t1 <*> makeSlot t2)
    ForallC _ _ _ -> fail "Existentials aren't supported."
  where makeSlot (_,ty) = pure (Nothing, ty)
        makeField (name,_,ty) =
          pure (Just (ValueVariable name), ty)
