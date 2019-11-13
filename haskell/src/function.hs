-- Functions implemented in Haskell
-- 
-- @author Gustavo Alves

main = undefined

data Transaction = Int

-- Filter transactions by year
filterByYear :: [Transaction] -> Int -> [Transaction]
filterByYear = undefined

-- Filter transactions by month and year
filterByYearMonth :: [Transaction] -> Int -> Int -> [Transaction]
filterByYearMonth = undefined

-- Calculate income by month and year
income :: [Transaction] -> Int -> Int -> Float
income = undefined

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

