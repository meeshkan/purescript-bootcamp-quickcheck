module Main where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements)

reverse :: forall a. List a -> List a
reverse Nil = Nil

reverse (Cons a b) = reverse b <> (Cons a Nil)

{-
class Arbitrary a where
  arbitrary = Gen a
-}
data MyAwesomeData a
  = MyAwesomeData a a

instance arbitraryMyAwesomeData :: Arbitrary a => Arbitrary (MyAwesomeData a) where
  arbitrary = MyAwesomeData <$> arbitrary <*> arbitrary

myAwesomeAdditionFunction :: MyAwesomeData Int -> Int
myAwesomeAdditionFunction (MyAwesomeData a b) = a + b

helloIzer :: String -> String
helloIzer a = "hello " <> a

data ClientType
  = Retail
  | Investment

genRetailPerson :: Gen Person
genRetailPerson = do
  name <- arbitrary
  country <- arbitrary
  balanceInEuroCents <- arbitrary
  address <- arbitrary
  clientType <- pure Retail
  pure
    { name, country, balanceInEuroCents, address, clientType
    }

genInvestmentPerson :: Gen Person
genInvestmentPerson = do
  name <- arbitrary
  country <- (Just <$> arbitrary)
  balanceInEuroCents <- arbitrary
  address <- arbitrary
  clientType <- pure Investment
  pure
    { name, country, balanceInEuroCents, address, clientType
    }

type Person
  = { name :: String
    , address :: Maybe String
    , country :: Maybe String
    , balanceInEuroCents :: Int
    , clientType :: ClientType
    }

mergeAccounts :: Person -> Person -> Person
mergeAccounts { name, address, country, balanceInEuroCents, clientType } t =
  { name, address, country, clientType, balanceInEuroCents: balanceInEuroCents - t.balanceInEuroCents
  }

main :: Effect Unit
main = do
  log "🍝"
