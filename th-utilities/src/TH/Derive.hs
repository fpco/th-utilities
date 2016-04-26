{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module TH.Derive where

import Data.Data
import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Instances
import TH.Utilities
import Data.Data
import Data.Generics

--TODO: support deriving on constraint kinds, for concision!

class Deriving (cls :: Constraint)

class Deriver (cls :: Constraint) where
    runDeriver :: Proxy cls -> Cxt -> Type -> Q [Dec]

class Instantiator (inst :: Constraint) where
    runInstantiator :: Proxy inst -> Cxt -> Type -> [Dec] -> Q [Dec]

derive :: DecsQ -> ExpQ
derive decsq = do
    -- TODO: warnings / errors for invalid derivers?
    -- ClassI _ insts <- reify ''Deriver
    -- let derivers = mapMaybe deriverInfo insts
    decs <- decsq
    let labeledDecs = zip (map (mkName . ("x" ++) . show) [0..]) decs
    doE $
        map toStmt labeledDecs ++
        [ noBindS [e| return $ concat $(listE (map (varE . fst) labeledDecs)) |] ]
  where
    toStmt (varName, InstanceD cxt (AppT (ConT ((== ''Deriving) -> True)) cls) []) =
        bindS (varP varName)
              [e| runDeriver $(proxyE (return cls))
                             cxt
                             cls |]
    toStmt (varName, InstanceD cxt ty decs) =
        bindS (varP varName)
              [e| runInstantiator $(proxyE (return ty))
                                  cxt
                                  ty
                                  decs |]
    toStmt (_, decl) = fail $
        "Expected deriver instance, instead got:\n" ++
        show decl

dequalifyMethods :: Data a => Name -> a -> Q a
dequalifyMethods className x = do
      info <- reify className
      case info of
          ClassI (ClassD _ _ _ _ decls) _ ->
              return (go [n | SigD n _ <- decls] x)
          info -> fail $ "dequalifyMethods expected class, but got " ++ show info
    where
      go :: Data b => [Name] -> b -> b
      go names = gmapT (go names) `extT` (id :: String -> String) `extT`
          (\n -> if n `elem` names then dequalify n else n)

{-
-- | Yields a function which can be used as a definition of
-- 'deriveInstances'. It expects to be passed an 'ExpQ'. This 'ExpQ'
deriverForClass
    :: Name
    -> ExpQ
    -> ExpQ
deriverForClass clsName innerExpr = do
    info <- reify clsName
    case info of
        ClassI
    [| \_ _ preds ty -> $(caseE [| ty |] ) |]
    case unAppsT headTy of
        (ConT cls:_) ->

  (AppT (ConT cls) ty)

deriver
    :: (Name -> [Type] -> [Dec] -> Q [Dec])
    -> (Proxy a -> Proxy cls -> Cxt -> Type -> Q [Dec])
derive f =
-}

{-
deriverInfo :: InstanceDec -> Maybe (Name, Name, Type)
deriverInfo (InstanceD _ (AppT (AppT (ConT ''Deriving) (ConT deriver)) cls)) =
     case unAppsT cls of
         (ConT clsName, _) -> Just (deriver, clsName, cls)
         _ -> Nothing
deriverInfo _ = Nothing
-}
