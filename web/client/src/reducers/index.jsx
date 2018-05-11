const initState = {
  mode: 'run',
  language: 'srl',
  code: 'int a\na += 1',
  result: {
    error: {},
    invert: '',
    translate: '',
    log: [],
    table: []
  }
}

const rootReducer = (state = initState, action) => {
  console.log("reducers")
  console.log(state,action);
  switch (action.type) {
    case 'change-mode':
      return { ...state, mode: action.payload }
    case 'change-language':
      return { ...state, language: action.payload }
    case 'change-code':
      return { ...state, code: action.payload }
    case 'change-invert-code':
      return { ...state,
        result: { ...state.result, error: {}, invert: action.payload }
      }
    case 'change-translate-code':
      return { ...state,
        result: { ...state.result, error: {}, translate: action.payload }
      }
    case 'change-result-error':
      return { ...state,
        result: { ...state.result, error: action.payload }
      }
    case 'change-result-table':
      return { ...state,
        result: { ...state.result, error: {}, table: action.payload }
      }
    default: return state

  }
}

export default rootReducer;
