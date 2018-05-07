export const runScript = (interp, code) => {
  type: 'run-script',
  payload: {
    interp,
    code
  }
}
