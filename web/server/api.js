import { Router, urlencoded, json } from 'express';
import { exec } from 'child_process';

// Setup
const api = Router();
api.use(json());

// Routes
api.post('/run/:lng',       (req,res) => { runMode('run',req,res) });
api.post('/run/log/:lng',   (req,res) => { runMode('run',req,res,true) });
api.post('/invert/:lng',    (req,res) => { runMode('invert',req,res) });
api.post('/translate/:lng', (req,res) => { runMode('translate',req,res) });
api.get('/template/list',   (req,res) => {
  exec('find templates -maxdepth 1 -type f \\( -iname "*.srl" -o -iname "*.rl"  \\)', (err,stdout) => {
    if (err) {
      res.json({
        type: 'error',
        message: 'Couldn\'t find templates: ' + err
      });
      return;
    }
    const files = stdout.trim().split("\n").map(s => s.replace('templates/',''))
    const rl    = files.filter(s => s.match(".*\\.rl"))
    const srl   = files.filter(s => s.match(".*\\.srl"))
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
      res.json({
        type: 'error',
        message: 'Couldn\'t fetch template ' + req.params.file + ': ' + err
      });
      return;
    }
    res.json({
      type: 'template',
        file: req.params.file,
      code: stdout
    });
  });
});

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
    res.json({
      type: 'error',
      message: '\'' + mode + '\' is an illegal mode.'
    });
    return;
  }
  const lng = req.params.lng;
  // Verify that lng is either rl or srl
  if (!verifyLng(lng)) {
    res.json({
      type: 'error',
      message: 'Language should either be rl or srl. Recieved ' + lng + '.'
    });
    return;
  }
  // Check if has code attribute
  const cnt = req.body;
  if (!('code' in cnt)) {
    res.json({
      type: 'error',
      message: 'Attribute \'code\' is missing in request.'
    });
    return;
  }
  // Execute code
  const flags = 'cj' + (log ? 'l' : '');
  const cmd = './bin/' + lng + ' '+mode+' "' + cnt.code + '" -' + flags;
  const maxBufferSize = 1024 * 3000 // 3 MB 
  console.log('running cmd: '+cmd);
  exec(cmd, { maxBuffer: maxBufferSize }, (err,stdout) => {
    if (err) {
      res.json({
        type: 'error',
        message: 'Execution of code failed, with error: ' + err
      });
      return;
    }
    if (log) console.log(JSON.parse(stdout));
    res.send(stdout);
  });
}

export default api;
