import express from 'express';
import { join } from 'path';
import { exists } from 'fs';
import * as c from './colors';

// Create express server
const app = express();

const port = 3001;

// Enable CORS
app.use((req, res, next) => {
  res.header('Access-Control-Allow-Origin',  '*');
  res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
  next();
});

// Setup api
import api from './api';
app.use('/api', api);

// Setup web-interface
app.get('/', (req,res) => {
  console.log(c.call('\n' + new Date(Date.now()).toLocaleString() + ': Requested ' + req.originalUrl));
  const p = join(__dirname, 'client', 'index.html');
  exists(p, exs => {
    if (!exs) {
      res.send('Couldn\'t serve index page.');
      console.log(c.error('  Couldn\'t serve index page.'));
      return;
    }
    res.sendFile(p);
    console.log(c.success('  Response ended with success.'));
  });
});
app.get('/:f.:ext', (req,res) => {
  const p = join(__dirname, 'client', req.params.f + '.' + req.params.ext);
  exists(p, exs => {
    if (!exs) {
      res.send('Couldn\'t serve ' + req.originalUrl + '.');
      return;
    }
    res.sendFile(p);
  });
});
app.get('*', (req,res) => res.send('Couldn\'t find requested page (404).'));

// Start server
app.listen(port, () => console.log('Server has started.\nWeb interface is running at ' + c.url('http://localhost:' + port.toString()) + '.'));
