export const changeMode = mode => { return {
  type: 'change-mode',
  payload: mode
}};

export const changeLanguage = lng => { return {
  type: 'change-language',
  payload: lng
}};

export const changeCode = code => { return {
  type: 'change-code',
  payload: code
}};

export const changeResultCode = (mode,code) => { return {
  type: 'change-' + mode + '-code',
  payload: code
}};

export const changeResultError = error => { return {
  type: 'change-result-error',
  payload: error
}};

export const changeResultTable = table => { return {
  type: 'change-result-table',
  payload: table
}};
