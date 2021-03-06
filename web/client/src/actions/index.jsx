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

export const changeResultLog = log => { return {
  type: 'change-result-log',
  payload: log
}};

export const startStepping = () => { return {
  type: 'stepping-start'
}};

export const stopStepping = () => { return {
  type: 'stepping-stop'
}};

export const nextStep = () => { return {
  type: 'stepping-next'
}};

export const prevStep = () => { return {
  type: 'stepping-prev'
}};

export const showSaveModal = () => { return {
  type: 'show-save-modal'
}};

export const hideSaveModal = () => { return {
  type: 'hide-save-modal'
}};

export const showOpenModal = () => { return {
  type: 'show-open-modal'
}};

export const hideOpenModal = () => { return {
  type: 'hide-open-modal'
}};

export const changeTheme = theme => { return {
  type: 'change-theme',
  payload: theme
}};
