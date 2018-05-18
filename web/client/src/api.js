export function run(lng,content,callback,log=false) {
  const hasLog = log ? 'log/' : '';
  fetch(API_URL + '/run/' + hasLog + lng, {
    method: 'POST',
    body:   JSON.stringify(content),
    headers: {
      'Content-Type': 'application/json'
    }
  })
    .then(res => res.json())
    .then(
      (result) => {
        if (result.type=='error') callback(result);
        else                      callback(null,result);
      }, (error) => {
        callback({ type: 'error', message: error });
      });
}

export function invert(lng,content,callback) {
  fetch(API_URL + '/invert/' + lng, {
    method: 'POST',
    body:   JSON.stringify(content),
    headers: {
      'Content-Type': 'application/json'
    }
  })
    .then(res => res.json())
    .then(
      (result) => {
        if (result.type=='error') callback(result);
        else                      callback(null,result);
      }, (error) => {
        callback({ type: 'error', message: error });
      });
}

export function translate(lng,content,callback) {
  fetch(API_URL + '/translate/' + lng, {
    method: 'POST',
    body:   JSON.stringify(content),
    headers: {
      'Content-Type': 'application/json'
    }
  })
    .then(res => res.json())
    .then(
      (result) => {
        if (result.type=='error') callback(result);
        else                      callback(null,result);
      }, (error) => {
        callback({ type: 'error', message: error });
      });
}

export function templates(callback) {
  fetch(API_URL + '/template/list', {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json'
    }
  })
    .then(res => res.json())
    .then(
      (result) => {
        callback(null,result);
      }, (error) => {
        callback({ type: 'error', message: error });
      });
}

export function template(file,callback) {
  fetch(API_URL + '/template/' + encodeURIComponent(file), {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json'
    }
  })
    .then(res => res.json())
    .then(
      (result) => {
        callback(null,result);
      }, (error) => {
        callback({ type: 'error', message: error });
      });
}
