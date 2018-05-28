import { Router, urlencoded, json } from 'express';
import { exec } from 'child_process';
import * as c from './colors';

// Constants
const maxBuffer = 1024 * 3000; // 3 MB
const timeout   = 1000; // 1 s

// Setup
const api = Router();
api.use(json());

// Log requests
api.use((req,res,next) => {
  console.log(c.call('\n' + new Date(Date.now()).toLocaleString() + ': Requested ' + req.originalUrl));
  next();
});

// Routes
api.post('/run/:lng',       (req,res) => { runMode('run',req,res) });
api.post('/run/log/:lng',   (req,res) => { runMode('run',req,res,true) });
api.post('/invert/:lng',    (req,res) => { runMode('invert',req,res) });
api.post('/translate/:lng', (req,res) => { runMode('translate',req,res) });
api.get('/template/list',   (req,res) => {
  exec('find templates -maxdepth 1 -type f \\( -iname "*.srl" -o -iname "*.rl"  \\)', (err,stdout) => {
    if (err) {
      console.log(c.error('  Execution failed with: ' + handleError(err.message)));
      res.json({
        type: 'error',
        message: 'Couldn\'t find templates: ' + err
      });
      return;
    }
    const files = stdout.trim().split("\n").map(s => s.replace('templates/',''))
    const rl    = files.filter(s => s.match(".*\\.rl"))
    const srl   = files.filter(s => s.match(".*\\.srl"))
    console.log(c.success('  Response ended with success.'));
    res.json({
      type: 'templates',
      rl,
      srl
    });
  });
});
api.get('/template/:file',   (req,res) => {
  exec('cat "templates/' + encodeURIComponent(req.params.file) + '"', (err,stdout) => {
    if (err) {
      console.log(c.error('  Execution failed with: ' + handleError(err.message)));
      res.json({
        type: 'error',
        message: 'Couldn\'t fetch template ' + req.params.file + ': ' + err
      });
      return;
    }
    console.log(c.success('  Response ended with success.'));
    res.json({
      type: 'template',
      file: req.params.file,
      code: stdout
    });
  });
});
// 404
api.get('/', _404);
api.get('/:nonsense', _404);
function _404(req,res) {
  console.log(c.error('  Unknown API-call.'));
  const nonsense = req.params.nonsense ? req.params.nonsense : '';
  res.json({
    type: 'error',
    message: '\'' + nonsense + '\' is unknown API-call.'
  });
}

// Helpers
function verifyLng(lng) {
  switch (lng) {
    case 'srl': return true;
    case 'rl':  return true;
    default:    return false;
  }
}

function verifyMode(mode) {
  switch (mode) {
    case 'run':       return true;
    case 'invert':    return true;
    case 'translate': return true;
    default:          return false;
  }
}

function runMode(mode, req, res, log=false) {
  // Verify mode
  if (!verifyMode(mode)) {
    console.log(c.error('  Execution failed with: ' + '\'' + mode + '\' is an illegal mode.'));
    res.json({
      type: 'error',
      message: '\'' + mode + '\' is an illegal mode.'
    });
    return;
  }
  const lng = req.params.lng;
  // Verify that lng is either rl or srl
  if (!verifyLng(lng)) {
    console.log(c.error('  Execution failed with: ' + 'Language should either be rl or srl. Recieved ' + lng + '.'));
    res.json({
      type: 'error',
      message: 'Language should either be rl or srl. Recieved ' + lng + '.'
    });
    return;
  }
  // Check if has code attribute
  const cnt = req.body;
  if (!('code' in cnt)) {
    console.log(c.error('  Execution failed with: ' + 'Attribute \'code\' is missing in request.'));
    res.json({
      type: 'error',
      message: 'Attribute \'code\' is missing in request.'
    });
    return;
  }
  // Execute code
  const flags = 'cj' + (log ? 'l' : '');
  const cmd = './bin/' + lng + ' '+mode+' "' + cnt.code + '" -' + flags;
  exec(cmd, { maxBuffer, timeout }, (err,stdout) => {
    if (err) {
    console.log(c.error('  Execution failed with: ' + handleError(err.message)));
      res.json({
        type: 'error',
        message: 'Execution of code failed with error: ' +  handleError(err.message)
      });
      return;
    }
    console.log(c.success('  Response ended with success.'));
    res.send(stdout);
  });
}

function handleError(err) {
  // Check if buffer overflow
  if (err.indexOf('maxBuffer') !== -1) {
    return 'Size of response exceeded ' + (maxBuffer/(1024)) + ' KB.';
  }
  return 'Timeout in execution occured. Infinite loops are a plausible cause.';
}

export default api;
