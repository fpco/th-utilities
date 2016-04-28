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
