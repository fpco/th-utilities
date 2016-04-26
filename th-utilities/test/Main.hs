{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

import TH.Derive
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


main = do
   print Foo
   print X
   print (Y == Z)
   print (Z == Z)
