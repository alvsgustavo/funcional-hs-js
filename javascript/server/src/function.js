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
  const filtered = filterByMonth(transactions, year, month);
  const ldm = lastDayOfMonth(year, month);
  const cf = [];
  cf[0] = {
    day: 1,
    balance: startingBalance(transactions, year, month) + dailyNetto(filtered, year, month, 1)
  };
  for (let i = 1; i < ldm + 1; i++) {
    cf[i] = {
      day: i + 1,
      balance: cf[i - 1].balance + dailyNetto(filtered, year, month, i + 1)
    };
  };
  return cf;
};

function maximumBalance (transactions, year, month) {
  const dailyBalances = cashFlow(transactions, year, month);
  const reducer = (acc, b) => (b.balance > acc.balance)? b : acc;
  return dailyBalances.reduce(reducer, dailyBalances[0]);
};

function minimumBalance (transactions, year, month) {
  const dailyBalances = cashFlow(transactions, year, month);
  const reducer = (acc, b) => (b.balance < acc.balance)? b : acc;
  return dailyBalances.reduce(reducer, dailyBalances[0]);
}

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

function lastDayOfMonth(year, month) {
  let lastDay;
  const months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  
  if (month == '01' && (year % 100 != 0 && year % 4 == 0 || year % 400 == 0)) lastDay = 29;
  else lastDay = months[parseInt(month)];
  return lastDay;
}

function dailyNetto (transactions, year, month, day) {
  const filtered = transactions.filter(t => {
    return t.data.year == year &&
      t.data.month == month &&
      t.data.dayOfMonth == day &&
      !t.tipos.includes('SALDO_CORRENTE') &&
      !t.tipos.includes('APLICACAO') &&
      !t.tipos.includes('VALOR_APLICACAO');
  });
  return filtered.reduce(((acc, t) => acc + t.valor), 0);
}

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