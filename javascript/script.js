'use strict';

//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
//////////////// Parte com as funções ////////////////
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////

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
  for (let i = 2; i < ldm + 1; i++) {
    cf[i - 1] = {
      day: i,
      balance: cf[i - 2].balance + dailyNetto(filtered, year, month, i)
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

function lastDayOfMonth(year, month) {
  let lastDay;
  const months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  
  if (month == '01' && (year % 100 != 0 && year % 4 == 0 || year % 400 == 0)) lastDay = 29;
  else lastDay = months[parseInt(month)];
  return lastDay;
};

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
};

///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
//////////////// Parte de visualização ////////////////
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////

let transactions;
fetch('http://150.165.15.10:8080/todasTransacoes', {method: 'POST'})
    .then(res => res.json())
    .then((out) => {
        transactions = out;
}).catch(err => console.error(err));

function cleanResultBox() {
  const $result = document.querySelector('.result');
  $result.innerHTML = '';
};


function fby(year) {
  //cleanResultBox();
  const rs = filterByYear(transactions, year);
  const $box = document.querySelector('.result');
  const $length = document.createElement('p');
  $length.innerHTML = `${rs.length} resultado(s) filtrado(s).`;
  $box.appendChild($length);
  
  rs.forEach(r => {
    const $r = document.createElement('div');
    $r.className = 'result-row';
    $r.innerHTML = `<p class='att'><strong>Data</strong>: ${r.data.dayOfMonth}/${r.data.month + 1}/${r.data.year}</p>
                    <p class='att'><strong>Nº DOC</strong>: ${r.numeroDOC}</p>
                    <p class='att'><strong>Valor:</strong> ${r.valor}</p>
                    <p class='att'><strong>Texto Identificador</strong>: ${r.textoIdentificador}</p>
                    <p class='att'><strong>Descrição:</strong> ${r.descricao}</p>
                    <p class='att'><strong>Tipos:</strong> ${r.tipos}</p>`;
    $box.appendChild($r);
  });
};

function fbm(year, month) {
  const rs = filterByMonth(transactions, year, month);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $length = document.createElement('p');
  $length.innerHTML = `${rs.length} resultado(s) filtrado(s).`;
  $box.appendChild($length);
  
  rs.forEach(r => {
    const $r = document.createElement('div');
    $r.className = 'result-row';
    $r.innerHTML = `<p class='att'><strong>Data</strong>: ${r.data.dayOfMonth}/${r.data.month + 1}/${r.data.year}</p>
                    <p class='att'><strong>Nº DOC</strong>: ${r.numeroDOC}</p>
                    <p class='att'><strong>Valor:</strong> ${r.valor}</p>
                    <p class='att'><strong>Texto Identificador</strong>: ${r.textoIdentificador}</p>
                    <p class='att'><strong>Descrição:</strong> ${r.descricao}</p>
                    <p class='att'><strong>Tipos:</strong> ${r.tipos}</p>`;
    $box.appendChild($r);
  });
};

function inc(year, month) {
  const result = income(transactions, year, month);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
  $result.innerText = `A receita para este mês foi de ${result}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function ex(year, month) {
  const result = expenses(transactions, year, month);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
  $result.innerText = `As despesas para este mês foram de ${result}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function net(year, month) {
  const net = netIncome(transactions, year, month);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (net) {
  $result.innerText = `A sobra para este mês foi de ${net}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function bal(year, month) {
  const result = balance(transactions, year, month);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
  $result.innerText = `O saldo deste mês foi de ${result}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function avgi(year) {
  const result = averageIncome(transactions, year);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
  $result.innerText = `A receita média deste ano é de ${result}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function avgi(year) {
  const result = averageIncome(transactions, year);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
  $result.innerText = `A receita média mensal deste ano é de ${result}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function avge(year) {
  const result = averageExpense(transactions, year);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
  $result.innerText = `A despesa média mensais deste ano é de ${result}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function avgn(year) {
  const result = averageNetIncome(transactions, year);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
  $result.innerText = `A sobra média mensal deste ano é de ${result}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function mx(year, month) {
  const result = maximumBalance(transactions, year, month);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
    $result.innerText = `Saldo máximo: ${result.balance}; Dia: ${result.day}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function mn(year, month) {
  const result = minimumBalance(transactions, year, month);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $result = document.createElement('p');
  if (result) {
  $result.innerText = `Saldo mínimo: ${result.balance}; Dia: ${result.day}`;
  } else {
    $result.innerText = "Não há transações para este período.";
  };

  $box.appendChild($result);
};

function flow(year, month) {
  const rs = cashFlow(transactions, year, month);
  cleanResultBox();
  const $box = document.querySelector('.result');
  const $desc = document.createElement('p');
  $desc.innerText = 'Fluxo de caixa para o mês';
  $box.appendChild($desc);
  
  rs.forEach(r => {
    const $r = document.createElement('div');
    $r.className = 'result-row';
    $r.innerHTML = `<p class='data'><strong>Dia: </strong>${r.day}</p>
                    <p class='att'><strong>Saldo: </strong>${r.balance}</p>`;
    $box.appendChild($r);
  });
};

function execute () {
  const f = document.querySelector('#op').value;
  const m = document.querySelector('#mes').value;
  const y = document.querySelector('#ano').value;
  try {
    switch (f) {
      case 'fby': fby(y); break;
      case 'fbm': fbm(y, m); break;
      case 'in': console.log('oi'); inc(y, m); break;
      case 'ex': ex(y, m); break;
      case 'net': net(y, m); break;
      case 'bal': bal(y, m); break;      
      case 'max': mx(y, m); break;
      case 'min': mn(y, m); break;
      case 'avgi': avgi(y); break;
      case 'avge': avge(y); break;     
      case 'avgn': avgn(y); break;
      case 'flow': flow(y, m); break;
    };
  } catch (err) {
    cleanResultBox();
    const $box = document.querySelector('.result');
    const $p = document.createElement('p');
    $p.innerText = 'Não há dados suficientes para este mês.';
    console.log(err);
    $box.appendChild($p);
  }
};