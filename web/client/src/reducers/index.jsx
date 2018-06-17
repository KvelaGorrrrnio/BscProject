const initState = {
  theme: 'default',
  mode: 'run',
  language: 'srl',
  code: '// Compute the n\'th fibonacci pair\n\nint n\nint v int w\n\nn ^= 16\nw ^= 1\nfrom (v = 0) do\n  v += w\n  swap v w\n  n -= 1\nloop .\nuntil (n = 0 || v > w)\n',
  stepState: {
    stepping: false,
    index: 0,
  },
  result: {
    error: {},
    invert: '',
    translate: '',
    log: {
      state: [],
      table: [],
    },
    table: [],
  },
  modal: {
    save: false,
    open: false,
    help: false,
  },
}

const rootReducer = (state = initState, action) => {
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
    case 'change-result-log':
      return { ...state,
        result: { ...state.result,
          error: {},
          log: {
            state: action.payload.state.table,
            table: action.payload.table,
          }
        }
      }
    case 'stepping-start':
      return { ...state,
        stepState: { ...state.stepState, stepping: true, index: 0 }
      }
    case 'stepping-stop':
      return { ...state,
        stepState: { ...state.stepState, stepping: false, index: 0 }
      }
    case 'stepping-next':
      return { ...state,
        stepState: { ...state.stepState, index: state.stepState.index + 1 }
      }
    case 'stepping-prev':
      return { ...state,
        stepState: { ...state.stepState, index: state.stepState.index - 1 }
      }
    case 'show-save-modal':
      return { ...state,
        modal: { ...state.modal,
          save: true,
        }
      }
    case 'hide-save-modal':
      return { ...state,
        modal: { ...state.modal,
          save: false,
        }
      }
    case 'show-open-modal':
      return { ...state,
        modal: { ...state.modal,
          open: true,
        }
      }
    case 'hide-open-modal':
      return { ...state,
        modal: { ...state.modal,
          open: false,
        }
      }
    case 'show-help-modal':
      return { ...state,
        modal: { ...state.modal,
          help: true,
        }
      }
    case 'hide-help-modal':
      return { ...state,
        modal: { ...state.modal,
          help: false,
        }
      }
    case 'change-theme':
      localStorage.theme = action.payload;
      return { ...state,
        theme: action.payload,
      }
    default: return state
  }
}

export default rootReducer;
