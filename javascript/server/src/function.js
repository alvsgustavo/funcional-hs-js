'use strict';

/*
 * Implementation in functional Javascript
 *
 * @author Gustavo Alves
 */

function filterByYear (transactions, year) {
  return transactions.filter(t => t.data.year == year);
};

function filterByMonth (transactions, year, month) {
  return transactions.filter(t => t.data.year == year && t.data.month == month);
};

function income (transactions, year, month) {
  const filtered = transactions.filter(t => {
    return t.data.year == year &&
      t.data.month == month &&
      t.valor > 0 &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  return filtered.reduce(((acc, t) => acc + t.valor), 0);
};

function expenses (transactions, year, month) {
  const filtered = transactions.filter(t => {
    return t.data.year == year &&
      t.data.month == month &&
      t.valor < 0 &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  return filtered.reduce(((acc, t) => acc + t.valor), 0);
};

function netIncome (transactions, year, month) {
  const filtered = transactions.filter(t => {
    return t.data.year == year &&
      t.data.month == month &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  return filtered.reduce(((acc, t) => acc + t.valor), 0);
};

function balance (transactions, year, month) {
  const initialBalance = startingBalance(transactions, year, month);
  const nettoMonth = netIncome(transactions, year, month);
  return initialBalance + nettoMonth;
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
  const filtered = transactions.filter(t => {
    return t.data.year == year &&
      t.valor > 0 &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  const nom = numberOfMonths(transactions, year);
  return filtered.reduce(((acc, t) => acc + t.valor), 0) / nom;
};

function averageExpense (transactions, year) {
  const result = transactions.filter(t => {
    return t.data.year == year &&
      t.valor < 0 &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  const nom = numberOfMonths(transactions, year);
  return result.reduce(((acc, t) => acc + t.valor), 0) / nom;
};

function averageNetIncome (transactions, year) {
  const result = transactions.filter(t => {
    return t.data.year == year &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  const nom = numberOfMonths(transactions, year);
  return result.reduce(((acc, t) => acc + t.valor), 0) / nom;
};

function cashFlow (transactions, year, month) {
  // TODO
};

function startingBalance (transactions, year, month) {
  const result = transactions.find(t => {
    return t.data.year == year &&
      t.data.month == month &&
      t.data.dayOfMonth == 1 &&
      t.tipos.includes('SALDO_CORRENTE');
  });
  return result.valor;
};

function numberOfMonths (transactions, year) {
  let min = 11;
  let max = 0;
  
  transactions.forEach(t => {
    if (t.data.month < min) min = t.data.month;
    if (t.data.month > max) max = t.data.month; 
  });
  return max + min + 1;
};

function lastDayOfMonth (year) 

module.exports = {
  filterByYear,
  filterByMonth,
  income,
  expenses,
  netIncome,
  balance,
  maximumBalance,
  minimumBalance,
  averageIncome,
  averageExpense,
  averageNetIncome,
  cashFlow
}