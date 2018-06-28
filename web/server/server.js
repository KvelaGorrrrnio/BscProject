import express from 'express';
import { join } from 'path';
import { exists } from 'fs';
import * as c from './colors';

// Create express server
export const app = express();
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

// Setup help
app.use('/help', express.static('docs'));

// Setup web-interface
app.get('/', (req,res) => {
  console.log(c.call('\n' + new Date(Date.now()).toLocaleString() + ': Requested ' + req.originalUrl));
  const p = join(__dirname, 'client', 'index.html');
  _sendFile(p, res);
});
app.get('/:filename.:ext', (req,res) => {
  const p = join(__dirname, 'client', req.params.filename + '.' + req.params.ext);
  _sendFile(p, res, false);
});
app.get('*', (req,res) => res.status(404).sendFile(join(__dirname, 'static', '404.html')));

function _sendFile(p, res, log=true) {
  exists(p, exs => {
    if (!exs) {
      if (log) {
        console.log(c.error('  404 encountered.'));
      }
      res.status(404).sendFile(join(__dirname, 'static', '404.html'));
      return;
    }
    res.sendFile(p);
  });
}

// Start server
app.listen(port, () => {
  console.log('Server has started.\nWeb interface is running at ' + c.url('http://localhost:' + port.toString()) + '.');
});
