const initState = {
  running: false,
  interp: 'srl'
}

const rootReducer = (state = initState, action) => {
  switch (action.type) {
    case 'run-script':
      console.log('ran '+interp+' with:\n'+code);
      return state;
    default:
      return state;
  }
};

export default  rootReducer;
