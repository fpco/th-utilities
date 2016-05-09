{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

module TH.Derive.Internal (Deriving, Deriver(..), Instantiator(..)) where

import Data.Proxy (Proxy)
import GHC.Exts (Constraint)
import Language.Haskell.TH (Q, Dec, Cxt, Type)

-- | This class has no instances. Its only purpose is usage within the
-- @[d| ... |]@ quote provided to 'derive'. Usage such as @instance
-- Deriving (Foo X)@ indicates that you would like to use the 'Deriver'
-- registered for @Foo a@.
class Deriving (cls :: Constraint) where
    -- Un-exported method, to prevent this class from being
    -- instantiated.
    _noInstances :: cls => ()

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
-- definitions which specify how the generated instances behave. For
-- example, lets say we want to be able to directly define 'Eq' and
-- 'Ord' instances via a conversion function to the type to compare.
-- Here's what this currently looks like:
--
-- @
-- class Ord o => InstEqOrdVia o a where
--     _toOrd :: a -> o
--
-- instance Instantiator (InstEqOrdVia o a) where
--     runInstantiator _ preds (AppT (AppT (ConT ((== ''InstEqOrdVia) -> True)) _oTy) aTy) decls =
--         dequalifyMethods ''InstEqOrdVia =<<
--         sequence
--         [instanceD (return preds) [t| Eq $(return aTy) |] $
--             [valD (varP '(==))
--                   (normalB [| \l r -> _toOrd l == _toOrd r |])
--                   (map return decls)]
--         , instanceD (return preds) [t| Ord $(return aTy) |] $
--             [valD (varP 'compare)
--                   (normalB [| \l r -> compare (_toOrd l) (_toOrd r) |])
--                   (map return decls)
--             ]
--         ]
--     runInstantiator _ _ _ _ =
--         fail "Theoretically impossible case in InstEqOrdVia instantiator"
-- @
--
-- Why the underscore prefixing of @_toOrd@? It's to suppress name
-- shadowing warnings which otherwise occur. In the future, this library
-- will likely provide pretty ways to define instantiators. For now it's
-- a bit ugly.
--
-- Here's what usage of this looks like:
--
-- @
-- data T = Y | Z
--
-- $($(derive [d|
--     instance InstEqOrdVia Bool T where
--         _toOrd Y = True
--         _toOrd Z = False
--     |]))
--
-- main = when (Y > Z) (putStrLn "It worked!!")
-- @
class Instantiator (inst :: Constraint) where
    runInstantiator :: Proxy inst -> Cxt -> Type -> [Dec] -> Q [Dec]
