'use strict';

const express = require('express');
const app = express();
const routes = require('./src/routes');
const port = 3000;

app.use(express.json());
app.use(routes.router);

const helloWorld = (_, res) => res.send("Oi, mundo!");
const initLog = () => console.log(`Server started on http://127.0.0.1:${port}`);

app.get('/', helloWorld);
app.listen(port, initLog);