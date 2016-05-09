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
--     $($(derive [d|
--         instance Deriving (Storable X)
--         |]))
-- @
--
-- In particular, note the use of double splicing, @$($(derive [d| ...
-- |]))@. The inner @$(derive [d| ... |])@ expression generates code
-- which invokes the 'runDeriver' method with appropriate arguments. The
-- outer @$( ... $)@ then runs that code in order to generate the
-- resulting instances. This is how it does dispatch at compile time.
--
-- There are a number of advantages of re-using instance syntax in this
-- way:
--
-- * It allows the user to specify constraints. Similarly to GHC's need
-- for standalone deriving, it is sometimes very difficult for TH to
-- figure out appropriate superclass constraints.
--
-- * The instance gets thoroughly checked by GHC (syntax, kind, and type
-- checking). This means that you get reasonably nice error messages
-- when you misuse these.
--
-- * It allows the user to specify methods. With 'Instantiator's, the
-- user can provide values which can be used in the definition of the
-- generated instance. This is a bit like having
-- <https://ghc.haskell.org/trac/ghc/wiki/InstanceTemplates Instance
-- Templates>. We don't have pretty ways of writing these quite yet, but
-- I have worked on something
-- <https://github.com/mgsloan/instance-templates similar in the past>.
--
-- * Using compile-time dispatch allows for concise specification of a
-- multiple of instances you'd like derived.
--
-- * In the case of use of a 'Deriver's, the user doesn't need to know
-- about anything but 'derive' and the name of the class they want. (and
-- the 'Deriver' instance must be in scope one way or another)
module TH.Derive
    ( derive
    , Deriving
    , Deriver(..)
    , Instantiator(..)
    , dequalifyMethods
    ) where

import Data.Data
import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import TH.Utilities
import TH.Derive.Internal
import TH.Derive.Storable ()

--TODO: support deriving on constraint kinds, for concision!

-- | This is the primary function for users of "TH.Derive". See the
-- module documentation for usage info.
derive :: DecsQ -> ExpQ
derive decsq = do
    decs <- decsq
    let labeledDecs = zip (map (mkName . ("x" ++) . show) [(0::Int)..]) decs
    doE $
        map toStmt labeledDecs ++
        [ noBindS [e| return $ concat $(listE (map (varE . fst) labeledDecs)) |] ]
  where
    toStmt (varName, InstanceD preds (AppT (ConT ((== ''Deriving) -> True)) cls) []) =
        bindS (varP varName)
              [e| runDeriver $(proxyE (return cls))
                             preds
                             cls |]
    toStmt (varName, InstanceD preds ty decs) =
        bindS (varP varName)
              [e| runInstantiator $(proxyE (return ty))
                                  preds
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
          _ -> fail $ "dequalifyMethods expected class, but got:\n" ++ pprint info
    where
      go :: Data b => [Name] -> b -> b
      go names = gmapT (go names) `extT` (id :: String -> String) `extT`
          (\n -> if n `elem` names then dequalify n else n)

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
