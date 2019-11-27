import Test.HUnit
import Function
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.List
import GHC.Generics
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


testFilterByYear = TestCase $ assertEqual (length $ filterByYear ts 2018) 1146
testFilterByMonth = TestCase $ assertEqual (length $ filterByYearMonth ts 2018 0) 108
testIncome = TestCase $ assertEqual (income ts 2018 0) 104951.60
testExpenses = TestCase $ assertEqual (expenses ts 2018 0) -101378.97
testNetIncome = TestCase $ assertEqual (netIncome ts 2018 0) 3572.62
testBalance =  TestCase $ assertEqual (balance ts 2018 0) 47552.78
testMaxBalance = TestCase $ assertEqual (maximumBalance ts 2018 0) 64783.08
testMinBalance =  TestCase $ assertEqual (minimumBalance ts 2018 0) 4354.48
testAverageIncome = TestCase $ assertEqual (averageIncome ts 2018 0) 111459.98
testAverageExpenses = TestCase $ assertEqual (averageExpenses ts 2018 0) -104183.19
testAverageNetIncome = TestCase $ assertEqual (averageNetIncome ts 2018 0)  7276.7925

testList = TestList [TestLabel "1" testFilterByYear,
                     TestLabel "2" testFilterByMonth,
                     TestLabel "3" testIncome,
                     TestLabel "4" testExpenses,
                     TestLabel "5" testNetIncome,
                     TestLabel "6" testBalance,
                     TestLabel "7" testMaxBalance,
                     TestLabel "8" testMinBalance,
                     TestLabel "9" testAverageIncome,
                     TestLabel "10" testAverageExpenses,
                     TestLabel "11" testAverageNetIncome]

main :: IO ()
main = do
  ts <- get
  runTestTT testlist
  return ()
