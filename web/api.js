import { Router, urlencoded, json } from 'express';
import { exec } from 'child_process';

const api = Router();

api.use(json());
api.post('/run/:lng', (req, res) => {
  const code = req.body;
  console.log(code);
  const exe = './bin/' + req.params.lng + ' run -cj "' + code.code + '"'
  console.log(exe);
  exec(exe, (err, stdout) => {
    console.log(err);
    console.log(stdout);
    res.send(stdout);
  });
});

export default api;
