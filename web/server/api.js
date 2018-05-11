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
  console.log('running cmd: '+cmd);
  exec(cmd, (err,stdout) => {
    if (err) {
      res.json({
        type: 'error',
        message: 'Execution of code failed, with error: ' + err
      });
    }
    res.send(stdout);
  });
}

export default api;
