'use strict';

const router = require('express').Router({ strict: true });
const dataset = require('../../../data/dataset.json');
const fun = require('./function');

const filterByYear = function (req, res) {
  const year = req.params.year;
  const result = fun.filterByYear(dataset, year);
  console.log(`Filtered ${result.length} result(s) with parametre year=${year}`);
  res.send({length: result.length, result});
};

const filterByMonth = function (req, res) {
  const year = req.params.year;
  const month = req.params.month;
  const result = fun.filterByMonth(dataset, year, month);
  console.log(`Filtered ${result.length} result(s) with parametres year=${year}, month=${month}`);
  res.send({length: result.length, result});
};

const income = function (req, res) {
  const year = req.params.year;
  const month = req.params.month;
  const result = fun.income(dataset, year, month);
  console.log(`Calculated income with parametres year=${year}, month=${month}`);
  res.send({result});
};

const expenses = function (req, res) {
  const year = req.params.year;
  const month = req.params.month;
  const result = Math.abs(fun.expenses(dataset, year, month));
  console.log(`Calculated expenses with parametres year=${year}, month=${month}`);
  res.send({result});
};

const netIncome = function (req, res) {
  const year = req.params.year;
  const month = req.params.month;
  const result = fun.netIncome(dataset, year, month);
  console.log(`Calculated netIncome with parametres year=${year}, month=${month}`);
  res.send({result});
};

const balance = function (req, res) {
  const year = req.params.year;
  const month = req.params.month;
  const result = fun.balance(dataset, year, month);
  console.log(`Calculated balance with parametres year=${year}, month=${month}`);
  res.send({result});
};

const averageIncome = function (req, res) {
  const year = req.params.year;
  const result = fun.averageIncome(dataset, year);
  console.log(`Calculated average income with parametre year=${year}`);
  res.send({result});
};

const averageExpense = function (req, res) {
  const year = req.params.year;
  const result = -1 * fun.averageExpense(dataset, year);
  console.log(`Calculated average expense with parametre year=${year}`);
  res.send({result});
};

const averageNetIncome = function (req, res) {
  const year = req.params.year;
  const result = fun.averageNetIncome(dataset, year);
  console.log(`Calculated average net income with parametre year=${year}`);
  res.send({result});
};

const cashFlow = function (req, res) {
  const year = req.params.year;
  const month = req.params.month;
  const result = fun.cashFlow(dataset, year, month);
  res.send(result);
};

const maximumBalance = function (req, res) {
  const year = req.params.year;
  const month = req.params.month;
  const result = fun.maximumBalance(dataset, year, month);
  res.send(result);
};

const minimumBalance = function (req, res) {
  const year = req.params.year;
  const month = req.params.month;
  const result = fun.minimumBalance(dataset, year, month);
  res.send(result);
};

/*
 * Routes
 */

router
  .get('/filter/:year', filterByYear)
  .get('/filter/:year/:month', filterByMonth)
  .get('/income/:year/:month', income)
  .get('/expenses/:year/:month', expenses)
  .get('/netIncome/:year/:month', netIncome)
  .get('/balance/:year/:month', balance)
  .get('/maximumBalance/:year/:month', maximumBalance)
  .get('/minimumBalance/:year/:month', minimumBalance)
  .get('/averageIncome/:year', averageIncome)
  .get('/averageExpense/:year', averageExpense)
  .get('/averageNetIncome/:year', averageNetIncome)
  .get('/cashFlow/:year/:month', cashFlow);

module.exports = {
  router,
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
};