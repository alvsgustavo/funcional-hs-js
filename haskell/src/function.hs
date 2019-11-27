{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.List
import GHC.Generics

main = undefined

get :: IO [Transaction]
get = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> error "Erro ao decodificar"
    Right ts -> return ts

data Date = Date { year :: Int
                 , month :: Int
                 , dayOfMonth :: Int
                 } deriving (Show, Generic)

data Transaction = Transaction { date :: Date
                               , textoIdentificador :: String
                               , valor :: Float
                               , descricao :: String
                               , numeroDOC :: String
                               , classificada :: Bool
                               , tipos :: [String]
                               , arquivos :: [String]
                               } deriving (Show, Generic)


instance FromJSON Date
instance ToJSON Date
instance FromJSON Transaction
instance ToJSON Transaction


jsonFile :: FilePath
jsonFile = "../../data/dataset.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


-- Filter transactions by year
filterByYear :: [Transaction] -> Int -> [Transaction]
filterByYear ts y = filter (\t -> checkYear t y) ts

-- Filter transactions by month and year
filterByYearMonth :: [Transaction] -> Int -> Int -> [Transaction]
filterByYearMonth ts y m = filter (\t -> checkYear t y && checkMonth t m) ts

-- Calculate income by month and year
income :: [Transaction] -> Int -> Int -> Float
income ts y m = foldl (+) 0 $ map valor $ filterIncome
                where filterIncome = filter (\t -> validTransaction t && checkYear t y && checkMonth t m && valor t > 0) ts

-- Calculate expenses by month and year
expenses :: [Transaction] -> Int -> Int -> Float
expenses ts y m = foldl (+) 0 $ map valor $ filterExpenses
                  where filterExpenses = filter (\t -> validTransaction t && checkYear t y && checkMonth t m && valor t < 0) ts

-- Calculate net income by month and year
netIncome :: [Transaction] -> Int -> Int -> Float
netIncome ts y m = foldl (+) 0 $ map valor $ filterByYearMonth ts y m

-- Calculate balance by month and year
balance :: [Transaction] -> Int -> Int -> Float
balance ts y m = initialBalance ts y m + netIncome ts y m

-- Calculate maximum balance in a specified month and year
maximumBalance :: [Transaction] -> Int -> Int -> Float
maximumBalance ts y m = maximum $ map (snd) $ cashFlow ts y m

-- Calculate minimum balance in a specified month and year
minimumBalance :: [Transaction] -> Int -> Int -> Float
minimumBalance ts y m = minimum $ map (snd) $ cashFlow ts y m

-- Calculate average income by year
averageIncome :: [Transaction] -> Int -> Float
averageIncome ts y = (foldl (+) 0 $ [valor t | t <- (filterByYear ts y), valor t > 0]) / 12

-- Calculate average expenses by year
averageExpenses :: [Transaction] -> Int -> Float
averageExpenses ts y = (foldl (+) 0 $ [valor t | t <- (filterByYear ts y), valor t < 0]) / 12

-- Calculate average net income by year
averageNetIncome :: [Transaction] -> Int -> Float
averageNetIncome ts y = (foldl (+) 0 $ map valor $ filterByYear ts y) / (12)

-- Return cash flow in a specified month/year
cashFlow :: [Transaction] -> Int -> Int -> [(Int, Float)]
cashFlow ts y m = [(d, (initialBalance ts y m) + dailyBalance ts y m d) | d <- [1..(1 + (dayOfMonthsInMonth m))]] 

-- Auxiliary functions
--
dailyBalance :: [Transaction] -> Int -> Int -> Int -> Float
dailyBalance ts y m d = sum $ map (valor) $ filter (\t -> checkYear t y && checkMonth t m && (dayOfMonth . date) t <= d && validTransaction t) ts 

checkYear :: Transaction -> Int -> Bool
checkYear t y = (year . date) t == y

checkMonth :: Transaction -> Int -> Bool
checkMonth t m = (month . date) t == m

checkdayOfMonth :: Transaction -> Int -> Bool
checkdayOfMonth t d = (dayOfMonth . date) t == d

validTransaction :: Transaction -> Bool
validTransaction t = let saldoCorrenteCheck = "SALDO_CORRENTE" `elem` (tipos t)
                         aplicacaoCheck = "APLICACAO" `elem` (tipos t)
                         valorAplicacaoCheck = "VALOR_APLICACAO" `elem` (tipos t)
                     in not saldoCorrenteCheck && not aplicacaoCheck && not valorAplicacaoCheck

initialBalance :: [Transaction] -> Int -> Int -> Float
initialBalance ts y m = let result = Data.List.find (\t -> checkYear t y && checkMonth t m && (dayOfMonth . date) t == 1 && "SALDO_CORRENTE" `elem` (tipos t)) ts
                        in case result of
                          Just x -> valor x
                          Nothing -> error "This wasn't supposed to happen"

dayOfMonthsInMonth :: Int -> Int
dayOfMonthsInMonth m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! m

