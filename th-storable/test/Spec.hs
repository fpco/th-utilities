{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad (when)
import           Data.Int
import qualified Data.Vector.Storable as SV
import           Foreign.Storable
import           Foreign.Storable.TH
import           TH.Derive
import           Test.Hspec

data ADataType
    = Con0
    | Con1 Int32
    | Con2 Int32 Int64
    deriving (Eq, Show)

$(derive [d| instance Deriving (Storable ADataType) |])

main :: IO ()
main = hspec $ describe "th-storable" $
    it "can roundtrip a data type" $ do
        roundTrips Con0
        roundTrips (Con1 minBound)
        roundTrips (Con1 0)
        roundTrips (Con1 maxBound)
        roundTrips (Con2 maxBound minBound)
        roundTrips (Con2 maxBound 0)
        roundTrips (Con2 maxBound maxBound)

roundTrips :: (Storable a, Show a, Eq a) => a -> IO ()
roundTrips x =
    when (SV.head (SV.singleton x) /= x) $
        fail ("Failed to roundtrip " ++ show x)
