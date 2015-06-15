{-# LANGUAGE ScopedTypeVariables #-}
module HTreesSpec where
import           Control.Applicative ((<|>))
import           Control.Applicative ((<$>))
import           Control.Applicative ((<*>))
import qualified Data.Vector         as V
import           HTrees
import           Test.Hspec          hiding (Example)
import           Test.QuickCheck

main :: IO ()
main = hspec spec

instance Arbitrary l => Arbitrary (Tree l) where
  arbitrary = sized tree'
    where tree' 0 = Leaf <$> arbitrary
          tree' n = (Node <$> arbitrary <*> tree' (n`div`2) <*> tree' (n`div`2))

instance Arbitrary l => Arbitrary (Model l) where
  arbitrary = Model <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary l => Arbitrary (Example l) where
  arbitrary = Ex <$> (uninst <$> arbitrary) <*> arbitrary
instance Arbitrary Split where
  arbitrary = Split <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Attribute where
  -- this is sketchy, but we don't know how big the row is,
  -- and we don't really care about correctness properties yet, only serialisability
  arbitrary = Attr <$> arbitrary <*> return 0

instance Arbitrary l => Arbitrary (FnSrc l) where
  arbitrary = Const <$> arbitrary

newtype Instanced = Instanced { uninst :: Instance}
instance Arbitrary Instanced where
  arbitrary = Instanced . V.fromList <$> arbitrary

spec :: SpecWith ()
spec = describe "HTrees" $ do
  it "can roundtrip tree representations" $ do
    -- this is weirdly slow without cutting the size to something reasonable.
    property (mapSize (const 10) (\(x::Tree Double) -> read (show x) == x))
