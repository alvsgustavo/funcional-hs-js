module Transaction
( Date
, Transaction
, TransactionType
) where

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
