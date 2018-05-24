import React, { Component } from 'react';
import { connect } from 'react-redux';
import * as actions from '../actions/index';
import * as api from '../api';
import { Controlled as CodeMirror } from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import '../styles/EditorTheme.scss';
import '../styles/Editor.scss';
import '../syntax.js';

class Editor extends Component {

  constructor(props) {
    super(props);
    // CodeMirror options
    this.options = {
      lineNumbers: true,
      lineWrapping: true,
      autofocus: true,
      indentUnit: 2,
      tabSize: 2,
      smartIndent: true,
      theme: 'editor-theme',
      mode: '[s]rl',
      extraKeys: {
        'Cmd-Enter': cm => {
          switch (this.props.mode) {
            case 'run': this.runProgram(); break;
            case 'step':      this.stepEnter(); break;
            case 'invert': this.invertProgram(); break;
            case 'translate': this.translateProgram(); break;
          }
        },
        'Shift-Cmd-Enter': cm => {
          switch (this.props.mode) {
            case 'step':      this.stepShiftEnter(); break;
          }
        },
        'Shift-Ctrl-Enter': cm => {
          switch (this.props.mode) {
            case 'step':      this.stepShiftEnter(); break;
          }
        },
        'Ctrl-Enter': cm => {
          switch (this.props.mode) {
            case 'run':       this.runProgram(); break;
            case 'step':      this.stepEnter(); break;
            case 'invert':    this.invertProgram(); break;
            case 'translate': this.translateProgram(); break;
          }
        },
        'Cmd-Backspace': cm => {
          if (this.props.stepState.stepping) {
            this.stopStepping();
          }
        },
        'Ctrl-Backspace': cm => {
          if (this.props.stepState.stepping) {
            this.stopStepping();
          }
        },
        'Tab': function (cm) {
            if (cm.somethingSelected()) {
                var sel = editor.getSelection('\n');
                // Indent only if there are multiple lines selected, or if the selection spans a full line
                if (sel.length > 0 && (sel.indexOf('\n') > -1 || sel.length === cm.getLine(cm.getCursor().line).length)) {
                    cm.indentSelection('add');
                    return;
                }
            }

            if (cm.options.indentWithTabs)
                cm.execCommand('insertTab');
            else
                cm.execCommand('insertSoftTab');
        },
        'Shift-Tab': function (cm) {
            cm.indentSelection('subtract');
        },
      }
    };
    // CodeMirror instance
    this.editor = null;
  }

  setEditor(editor) {
    this.editor = editor;
  }

  stopStepping() {
    this.props.changeResultLog({ state: { table: [] }, table: [] });
    this.props.stopStepping();
  }

  stepEnter() {
    if (!this.props.stepState.stepping) {
      this.runProgram(true);
    } else if (this.props.stepState.index < this.props.result.log.table.length) {
      this.props.nextStep();
    }
  }

  stepShiftEnter() {
    if (this.props.stepState.stepping && this.props.stepState.index <= 0) {
      return;
    }
    this.props.prevStep();
  }

  invertProgram() {
    api.invert(this.props.language,{
      code: this.props.code
    }, (err,result) => {
      if (err) this.props.changeResultError(err);
      else     this.props.changeResultCode('invert',result.code);
    });
  }

  translateProgram() {
    api.translate(this.props.language,{
      code: this.props.code
    }, (err,result) => {
      if (err) this.props.changeResultError(err);
      else     this.props.changeResultCode('translate',result.code);
    });
  }

  runProgram(log=false) {
    api.run(this.props.language,{
      code: this.props.code
    }, (err,result) => {
      if (err)     this.props.changeResultError(err);
      else if(log) {
        this.props.changeResultLog(result);
        this.props.startStepping();
      } else         this.props.changeResultTable(result.table);
    }, log);
  }

  getStepLine() {
    // return empty if index is out of bound
    if (this.props.stepState.index > this.props.result.log.table.length) {
      return 0;
    }
    for (var i=this.props.stepState.index - 1; i >= 0; i--) {
      if ( this.props.result.log.table[i] !== undefined
        && this.props.result.log.table[i].position  !== undefined
        && this.props.result.log.table[i].position.line !== undefined) {
        return parseInt(this.props.result.log.table[i].position.line);
      }
    }
    return 0;
  }
  stepFailed () {
    for (var i=0; i < this.props.result.log.table.length; i++) {
      if ( this.props.result.log.table[i] !== undefined
        && this.props.result.log.table[i].type == 'error') {
        return true;
      }
    }
    return false;
  }

  render() {
    const err = this.props.result.error;
    const options = this.options;
    options.readOnly = this.props.stepState.stepping;
    if (this.editor) {
      // Reset
      for (var i=0; i<this.editor.doc.size; i++) {
        this.editor.doc.removeLineClass(i, 'wrap', 'error');
        this.editor.doc.removeLineClass(i, 'wrap', 'step');
      }
      if ('position' in err) {
        const line = parseInt(err.position.line)-1;
        if (line > 0) {
          this.editor.doc.addLineClass(line, 'wrap', 'error');
          this.editor.scrollIntoView(line,60);
        }
      } else if (this.props.stepState.stepping) {
        const line = this.getStepLine()-1;
        if (line > 0) {
          const done = this.props.result.log.table.length > 0 && this.props.stepState.index >= this.props.result.log.table.length;
          this.editor.doc.addLineClass(line, 'wrap', this.stepFailed() && done  ? 'error' : 'step');
          if (line > 0) this.editor.scrollIntoView(line,60);
        }
      }
    }
    const className = (err.type ? 'editor-wrapper has-error' : 'editor-wrapper') + ' notranslate' + (options.readOnly ? ' disabled' : '');
    const Error = err.type ? (<div className='error-wrapper'>{err.message}</div>) : null;
    return (
      <div className={className}>
        <CodeMirror
          editorDidMount={this.setEditor.bind(this)}
          value={this.props.code}
          options={options}
          onBeforeChange={(editor, data, code) => {
            this.props.changeCode(code);
          }}
          onChange={(editor, value) => {}}
        />
        {Error}
      </div>
    );
  }
}

const mapDispatchToProps = dispatch => { return {
  changeCode:        code        => dispatch(actions.changeCode(code)),
  changeMode:        mode        => dispatch(actions.changeMode(mode)),
  changeLanguage:    language    => dispatch(actions.changeLanguage(language)),
  changeResultError: error       => dispatch(actions.changeResultError(error)),
  changeResultCode:  (mode,code) => dispatch(actions.changeResultCode(mode,code)),
  changeResultTable: table       => dispatch(actions.changeResultTable(table)),
  changeResultLog:   log         => dispatch(actions.changeResultLog(log)),
  startStepping:     ()          => dispatch(actions.startStepping()),
  stopStepping:      ()          => dispatch(actions.stopStepping()),
  stopStepping:      ()          => dispatch(actions.stopStepping()),
  nextStep:          ()          => dispatch(actions.nextStep()),
  prevStep:          ()          => dispatch(actions.prevStep())
}};

const mapStateToProps = state => { return {
  mode:      state.mode,
  language:  state.language,
  code:      state.code,
  stepState: state.stepState,
  result:    state.result
}};

export default connect(mapStateToProps,mapDispatchToProps)(Editor);
