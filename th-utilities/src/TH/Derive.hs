{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module implements a system for registering and using typeclass
-- derivers and instantiators. This allows you to derive instances for
-- typeclasses beyond GHC's ability to generate instances in @deriving@
-- clauses.
--
-- For exmaple, "TH.Derive.Storable" defines a 'Deriver' for 'Storable'.
-- This allows us to use 'derive' to generate an instance for Storable:
--
-- @
--     data X = X Int Float
--
--     $($(derive [d| instance Deriving (Storable X) |]))
-- @
--
-- In particular, note the use of double splicing - @$($(derive [d| ...
-- |]))@. The inner @$(derive [d| ... |])@ expression generates code
-- which invokes the 'runDeriver' method with appropriate arguments. The
-- outer @$( ... $)@ then runs that code in order to generate the
-- resulting instances. This is how we achieve dispatch at compile time.
--
-- The power of this approach is that the set of 'Deriver's and
-- 'Instantiator's are open. Users can just use 'derive' along with
-- normal instance syntax to cause instances to be generated.
module TH.Derive
    ( derive
    , Deriving
    , Deriver(..)
    , Instantiator(..)
    , dequalifyMethods
    ) where

import Data.Data
import Data.Generics
import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Instances
import TH.Utilities

--TODO: support deriving on constraint kinds, for concision!

-- | This class has no instances. Its only purpose is usage within the
-- @[d| ... |]@ quote provided to 'derive'. Usage such as @instance
-- Deriving (Foo X)@ indicates that you would like to use the 'Deriver'
-- registered for @Foo a@.
class Deriving (cls :: Constraint) where
    -- Un-exported method, to prevent this class from being
    -- instantiated.
    noInstances :: cls => ()

-- | Instances of 'Deriver' describe a default way of creating an
-- instance for a particular typeclass. For example, if I wanted to
-- write something that derives 'Eq' instances, I would write a
-- @instance Deriver (Eq a)@.
class Deriver (cls :: Constraint) where
    runDeriver :: Proxy cls -> Cxt -> Type -> Q [Dec]

-- | Instances of 'Instantiator' are similar in purpose to instance of
-- 'Deriver'. The difference is that instead of using the 'Deriving'
-- class, each instantiator has its own new typeclass. This means that
-- you can have multiple instantiators that all produce instances for
-- the same typeclass, using different approaches.
--
-- Having a new class also allows the instantiator to have methods and
-- data / type family declarations. This allows the user to provide
-- definitions which specify how the generated instance behaves.
class Instantiator (inst :: Constraint) where
    runInstantiator :: Proxy inst -> Cxt -> Type -> [Dec] -> Q [Dec]

-- | This is the primary function for users of "TH.Derive". See the
-- module documentation for usage info.
derive :: DecsQ -> ExpQ
derive decsq = do
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

-- | Useful function for defining 'Instantiator' instances. It uses
-- 'Data' to generically replace references to the methods with plain
-- 'Name's. This is handy when you are putting the definitions passed to
-- the instantiator in a where clause. It is also useful so that you can
-- reference the class methods from AST quotes involved in the
-- definition of the instantiator.
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
    -- Code originally from 'deriver'
    -- TODO: warnings / errors for invalid derivers?
    ClassI _ insts <- reify ''Deriver
    let derivers = mapMaybe deriverInfo insts

deriverInfo :: InstanceDec -> Maybe (Name, Name, Type)
deriverInfo (InstanceD _ (AppT (AppT (ConT ''Deriving) (ConT deriver)) cls)) =
     case unAppsT cls of
         (ConT clsName, _) -> Just (deriver, clsName, cls)
         _ -> Nothing
deriverInfo _ = Nothing
-}
