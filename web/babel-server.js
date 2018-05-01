// Enable ES6 via babel
require('babel-register')({
  presets: ['env'],
});
module.exports = require('./server.js');
