{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.UtilitiesSpec where

import Data.Typeable
import Language.Haskell.TH as TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift
import Test.Hspec as HS
import TH.Utilities


spec :: Spec
spec = do
  describe "TH.Utilities.typeRepToType" $ do
    it "Int" $ do
      tr (Proxy @Int) `shouldReturn` $( lift =<< [t| Int |] )
    it "Bool" $ do
      tr (Proxy @Bool) `shouldReturn` $( lift =<< [t| Bool |] )
    it "Maby Int" $ do
      tr (Proxy @(Maybe Int)) `shouldReturn` $(lift =<< [t| Maybe Int |])
    it "[Int]" $ do
      tr (Proxy @[Int]) `shouldReturn` $(lift =<< [t| [Int] |])
    it "(Bool,Int)" $ do
      tr (Proxy @(Bool, Int)) `shouldReturn` $(lift =<< [t| (Bool, Int) |])
    it "()" $ do
      tr (Proxy @()) `shouldReturn` $(lift =<< [t| () |])
    it "42" $ do
      tr (Proxy @42) `shouldReturn` $(lift =<< [t| 42 |])
    it "'c'" $ do
      tr (Proxy @'c') `shouldReturn` $(lift =<< [t| 'c' |])
    it "str" $ do
      tr (Proxy @"hello \"world") `shouldReturn` $(lift =<< [t| "hello \"world" |])
  where
    tr :: forall a. Typeable a => Proxy a -> IO Type
    tr p = runQ (typeRepToType (typeRep p))
