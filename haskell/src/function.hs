-- Functions implemented in Haskell
-- 
-- @author Gustavo Alves

--import Transaction
import Data.List

main = undefined

data Date = Date { year :: Int
                 , month :: Int
                 , day :: Int
                 } deriving (Show, Eq)

data Transaction = Transaction { date :: Date
                               , textoIdentificador :: String
                               , valor :: Float
                               , descricao :: String
                               , numeroDOC :: String
                               , classificada :: Bool
                               , tipos :: [TransactionType]
                               , arquivos :: [String]
                               } deriving (Eq, Show)

data TransactionType = SALDO_CORRENTE | VALOR_APLICACAO |
                       RECEITA_OPERACIONAL | TAXA_CONDOMINIO |
                       TAXA_EXTRA | TAXA_SALAO_FESTA |
                       MULTA_JUROS | TAXA_AGUA |
                       RECUPERACAO_ATIVOS | MULTA_JURO_CORRECAO_COBRANCA |
                       OUTRAS_RECEITAS | DESPESAS_PESSOAL |
                       TERCEIRIZACAO_FUNCIONARIOS | VIGILANCIA |
                       SALARIO_FUNCIONARIOS_ORGANICOS | ADIANTAMENTO_SALARIAL_FUNCIONARIOS_ORGANICOS |
                       FERIAS | INSS | APLICACAO deriving (Eq, Show)

-- Filter transactions by year
filterByYear :: [Transaction] -> Int -> [Transaction]
filterByYear ts y = filter (\t -> validTransaction t && checkYear t y) ts

-- Filter transactions by month and year
filterByYearMonth :: [Transaction] -> Int -> Int -> [Transaction]
filterByYearMonth ts y m = filter (\t -> validTransaction t && checkYear t y && checkMonth t m) ts

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
averageNetIncome ts y = (foldl (+) 0 $ map valor $ filterByYear ts y) / 12

-- Return cash flow in a specified month/year
cashFlow :: [Transaction] -> Int -> Int -> [(Int, Float)]
cashFlow ts y m = [(d, dailyBalance d) | d <- [1..daysInMonth+1]]
                  where
                    daysInMonth = (maximum . nub) $ map (day . date) $ filter (\t -> checkYear t y && checkMonth t m) ts
                    dailyBalance d = foldl (+) (initialBalance ts y m) $ map valor $ filter (\t -> checkYear t y && checkYear t m && (day . date) t <= d && validTransaction t) ts 

-- Auxiliary functions
--

checkYear :: Transaction -> Int -> Bool
checkYear t y = (year . date) t == y

checkMonth :: Transaction -> Int -> Bool
checkMonth t m = (month . date) t == m

checkDay :: Transaction -> Int -> Bool
checkDay t d = (day . date) t == d

validTransaction :: Transaction -> Bool
validTransaction t = let saldoCorrenteCheck = SALDO_CORRENTE `elem` (tipos t)
                         aplicacaoCheck = APLICACAO `elem` (tipos t)
                         valorAplicacaoCheck = VALOR_APLICACAO `elem` (tipos t)
                     in saldoCorrenteCheck && aplicacaoCheck && valorAplicacaoCheck

initialBalance :: [Transaction] -> Int -> Int -> Float
initialBalance ts y m = let result = find (\t -> checkYear t y && checkMonth t m && (day . date) t == 1 && SALDO_CORRENTE `elem` (tipos t)) ts
                        in case result of
                          Just x -> valor x
                          Nothing -> error "This wasn't supposed to happen"

