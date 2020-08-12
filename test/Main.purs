module Test.Main where

import Prelude
import Control.Lazy (fix)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Main (MyAwesomeData(..), Person, genInvestmentPerson, genRetailPerson, helloIzer, mergeAccounts, myAwesomeAdditionFunction, reverse)
import Test.QuickCheck (Result(..), mkSeed, quickCheck, quickCheck', quickCheckPure, quickCheckWithSeed)
import Test.QuickCheck.Gen (Gen)

foo :: forall a. a -> Unit -> a
foo a _ = a

--myNotMaybe :: Int
--myNotMaybe = (+) 0 myNotMaybe
myMaybe :: Unit -> Maybe Int
myMaybe = fix \p -> foo ((+) <$> Nothing <*> (myMaybe unit))

-- reverse (reverse a) == a
reverser :: List Int -> Boolean
reverser l = reverse (reverse l) == l

multiplicationIsReflexive :: Int -> Int -> Boolean
multiplicationIsReflexive a b = a * b == b * a

multiplicationIsDistributiveOverAddition :: Int -> Int -> Int -> Boolean
multiplicationIsDistributiveOverAddition a b c = a * (b + c) == (a * b) + (a * c)

additionIsReflexive :: MyAwesomeData Int -> Boolean
additionIsReflexive t@(MyAwesomeData a b) = myAwesomeAdditionFunction t == myAwesomeAdditionFunction (MyAwesomeData b a)

helloizerTest :: String -> Boolean
helloizerTest s = helloIzer s == "hello " <> s

mergeAccountsAddsBalances :: Gen Person -> Gen Person -> Gen Result
mergeAccountsAddsBalances gp0 gp1 = do
  p0 <- gp0
  p1 <- gp1
  let
    balanceSum = (mergeAccounts p0 p1).balanceInEuroCents
  let
    realSum = p0.balanceInEuroCents + p1.balanceInEuroCents
  pure
    ( if balanceSum == realSum then
        Success
      else
        Failed
          ( "You were supposed to add these two balances "
              <> (show p0.balanceInEuroCents)
              <> " "
              <> (show p1.balanceInEuroCents)
              <> " which should be "
              <> (show realSum)
              <> " but is actually, and most unfortunately "
              <> (show balanceSum)
          )
    )

class Testable' prop where
  test :: prop -> Gen Result

-- Int -> Int -> Int 
--mergeAccountsKeespFirstPersonName :: Person -> Person -> Boolean
--mergeAccountsKeespFirstPersonName p0 p1 = (mergeAccounts p0 p1).name == p0.name
main :: Effect Unit
main = do
  quickCheck reverser
  quickCheck additionIsReflexive
  quickCheck helloizerTest
  --quickCheckWithSeed (mkSeed 0) 1000 (mergeAccountsAddsBalances genRetailPerson genRetailPerson)
  --quickCheck (mergeAccountsAddsBalances genInvestmentPerson genInvestmentPerson)
  --log $ (show $ quickCheckPure (mkSeed 0) 10 (mergeAccountsAddsBalances genRetailPerson genRetailPerson))
  log $ (show $ quickCheckPure (mkSeed 0) 10 (mergeAccountsAddsBalances genInvestmentPerson genInvestmentPerson))
  quickCheck multiplicationIsReflexive
  quickCheck multiplicationIsDistributiveOverAddition

--quickCheck mergeAccountsKeespFirstPersonName
