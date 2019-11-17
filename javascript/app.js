'use strict';

/*
 * Implementation in functional Javascript
 *
 * @author Gustavo Alves
 */

const INCOME = 1;
const EXPENSE = 2;
const NET_INCOME = 3;
const defaultReducer = (acc, start) => acc.valor + start;

function filterByYear (transactions, year) {
  return transactions.filter(t => t.data.year == year);
};

function filterByMonth (transactions, year, month) {
  return transactions.filter(t => t.data.year == year && t.data.month == month);
};

function income (transactions, year, month) {
  const filtered = getNetIncomeTransactionsByMonth(transactions, year, month, INCOME);
  return filtered.reduce(defaultReducer);
};

function expenses (transactions, year, month) {
  const filtered = getNetIncomeTransactionsByMonth(transactions, year, month, EXPENSE);
  return filtered.reduce(defaultReducer);
};

function netIncome (transactions, year, month) {
  const filtered = getNetIncomeTransactionsByMonth(transactions, year, month, NET_INCOME);
  return filtered.reduce(defaultReducer);
};

function balance (transactions, year, month) {
  const initialBalance = startingBalance(transactions, year, month);
  return initialBalance + netIncome(transactions, year, month);
};

function maximumBalance (transactions, year, month) {
  const dailyBalances = cashFlow(transactions, year, month);
  const maximum = dailyBalances.reduce((acc, start) => {
    (acc.balance > start.balance)? acc : start;
  }, dailyBalances[0]);
  
  return maximum;
};

function minimumBalance (transactions, year, month) {
  // TODO
  const dailyBalances = cashFlow(transactions, year, month);
  const minimum = dailyBalances.reduce((acc, start) => {
    (acc.balance < start.balance)? acc : start;
  }, dailyBalances[0]);
  
  return minimum;
};

function averageIncome (transactions, year) {
  const filtered = getNetIncomeTransactions(transactions, year, INCOME);
  const total = filtered.reduce(defaultReducer);
  const average = total / 12;
  return average;
};

function averageExpenses (transactions, year) {
  const filtered = getNetIncomeTransactions(transactions, year, EXPENSE);
  const total = filtered.reduce(defaultReducer);
  const average = total / 12;
  return average;
};

function averageNetIncome (transactions, year, month) {
  const filtered = getNetIncomeTransactions(transactions, year, NET_INCOME);
  const total = filtered.reduce(defaultReducer);
  const average = total / 12;
  return average;
};

function cashFlow (transactions, year, month) {
  const thisMonth = getNetIncomeTransactionsByMonth(transactions, year, month);
  const cashFlow = []
  // Ensures it won't calculate a day in the future
  const getMaxDate = {
    (year == Date.getYear() && month == Date.getMonth()) - 1?\
      Date.getDate() : getLastDay(year, month);
  };
  for (i = 0; i < getMaxDate; i++) {
    const thisDay = getNetIncomeTransactionsByDay(thisMonth, year, month, day);
    const dailyTransactionsValue = thisDay.reduce(defaultReducer);
    cashFlow[i] = {day: i,
      balance: {
        cashFlow[i]? cashFlow[i].balance + (dailyTransactionsValue | 0)
        \: (i == 0? initialBalance(transactions, year, month) + (dailyTransactionsValue | 0)
        \: (dailyTransactionsValue | 0));
      };
    };
  };
  return cashFlow;
};

function getNetIncomeTransactionsByMonth (transactions, year, month, type) {
  const result = transactions.filter(t => {
    return t.data.year == year &&
      t.data.month == month &&
      isIt(type, t.valor) &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  return result;
};

function getNetIncomeTransactionsByYear (transactions, year, type) {
  const result = transactions.filter(t => {
    return t.data.year == year &&
      isIt(type, t.valor) &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  return result;
};

function getNetIncomeTransactionsByDay (transactions, year, month, day) {
  const result = transactions.filter(t => {
    return t.data.year == year &&
      t.data.month == month &&
      t.data.day == day &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  return result;
}

function isIt(type, value) {
  let result = undefined;

  switch (operator) {
    case INCOME:
      result = value >= 0;
      break;

    case EXPENSE:
      result = value < 0;
      break;

    case NET_INCOME:
      result = true;
      break;
  }
  return result;
};

function startingBalance (transactions, year, month) {
  return transactions.filter(t => {
    return t.data.year == year &&
      t.data.month == month &&
      t.data.day == 1 &&
      t.tipos.includes('SALDO_CORRENTE');
  })[0].valor;
};

function getLastDay (year, month) {
  const date = new Date(year, month, 0);
  return date.getDate();
};

