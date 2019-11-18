-- Functions implemented in Haskell
-- 
-- @author Gustavo Alves

--import Transaction

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
                       FERIAS | INSS deriving (Eq, Show)

-- Filter transactions by year
filterByYear :: [Transaction] -> Int -> [Transaction]
filterByYear ts y = filter (\x -> (year . date) x == y) ts

-- Filter transactions by month and year
filterByYearMonth :: [Transaction] -> Int -> Int -> [Transaction]
filterByYearMonth xs y m = filter (\x -> (month . date) x == m && (year . date) x == y) xs  

-- Calculate income by month and year
income :: [Transaction] -> Int -> Int -> Float
income xs y m = 

-- Calculate expenses by month and year
expenses :: [Transaction] -> Int -> Int -> Float
expenses = undefined

-- Calculate net income by month and year
netIncome :: [Transaction] -> Int -> Int -> Float
netIncome = undefined

-- Calculate balance by month and year
balance :: [Transaction] -> Int -> Int -> Float
balance = undefined

-- Calculate maximum balance in a specified month and year
maximumBalance :: [Transaction] -> Int -> Int -> Float
maximumBalance = undefined

-- Calculate minimum balance in a specified month and year
minimumBalance :: [Transaction] -> Int -> Int -> Float
minimumBalance = undefined

-- Calculate average income by year
averageIncome :: [Transaction] -> Int -> Float
averageIncome = undefined

-- Calculate average expenses by year
averageExpenses :: [Transaction] -> Int -> Float
averageExpenses = undefined

-- Calculate average net income by year
averageNetIncome :: [Transaction] -> Int -> Float
averageNetIncome = undefined

-- Return cash flow in a specified month/year
cashFlow :: [Transaction] -> Int -> Float
cashFlow = undefined

