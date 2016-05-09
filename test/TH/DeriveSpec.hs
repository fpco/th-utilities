{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module TH.DeriveSpec (spec) where

import TH.Derive
import TH.DeriveSpec.TH
import Test.Hspec

data Foo = Foo

data X = X

data T = Y | Z

$($(derive [d|
  instance InstShowBlind Foo

  instance InstShowConst X where
      _constResult _ = "wow!"

  instance InstEqOrdVia Bool T where
      _toOrd Y = True
      _toOrd Z = False
  |]))

spec :: SpecWith ()
spec = describe "Instantiators" $ do
    it "InstShowBlind" $ do
        show Foo `shouldBe` "ShowBlind"
    it "InstShowConst" $ do
        show X `shouldBe` "wow!"
    it "InstEqOrdVia" $ do
        (Y == Z) `shouldBe` False
        (Y > Z) `shouldBe` True
        (Z == Z) `shouldBe` True
