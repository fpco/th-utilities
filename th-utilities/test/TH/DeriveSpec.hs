{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module TH.DeriveSpec (spec) where

import TH.Derive
import Test.Hspec
import Test.TH

data Foo = Foo

data X = X

data Y = Y | Z

$($(derive [d|
  instance InstShowBlind Foo

  instance InstShowConst X where
      constResult _ = "wow!"

  instance InstEqBy Y Bool where
      toEq Y = True
      toEq Z = False
  |]))

spec = describe "Instantiators" $ do
    it "InstShowBlind" $ do
        show Foo `shouldBe` "ShowBlind"
    it "InstShowConst" $ do
        show X `shouldBe` "wow!"
    it "InstEqBy" $ do
        (Y == Z) `shouldBe` False
        (Z == Z) `shouldBe` True
