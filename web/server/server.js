import express from 'express';
import { join } from 'path';

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
app.use(express.static(join(__dirname, 'client')));
app.get('/', (req, res) => res.sendFile('index.html'));

// Start server
app.listen(port, () => console.log('Server has started!'));
