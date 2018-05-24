import React, { Component } from 'react';
import { connect } from 'react-redux';
import { changeMode, changeCode, changeLanguage, changeResultCode, changeResultError, changeResultTable, changeResultLog, changeStepping } from '../actions/index';
import * as api from '../api';
import { Controlled as CodeMirror } from 'react-codemirror2';
import '../mode/srl/srl';
import 'codemirror/lib/codemirror.css';
import './default-bsc.scss';
import './Editor.scss';

class Editor extends Component {

  constructor(props) {
    super(props);
    // CodeMirror options
    this.options = {
      lineNumbers: true,
      autofocus: true,
      indentUnit: 2,
      tabSize: 2,
      smartIndent: true,
      theme: 'default-bsc',
      mode: 'srl',
      extraKeys: {
        'Cmd-Enter' : cm => {
          switch (this.props.mode) {
            case 'run': this.runProgram(); break;
            case 'step':      this.runProgram(true); break;
            case 'invert': this.invertProgram(); break;
            case 'translate': this.translateProgram(); break;
          }
        },
        'Ctrl-Enter' : cm => {
          switch (this.props.mode) {
            case 'run':       this.runProgram(); break;
            case 'step':      this.runProgram(true); break;
            case 'invert':    this.invertProgram(); break;
            case 'translate': this.translateProgram(); break;
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
    api.run(this.props.language, {
      code: this.props.code
    }, (err,result) => {
      if (err)     this.props.changeResultError(err);
      else if(log) this.props.changeResultLog(result.log);
      else         this.props.changeResultTable(result.table);
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
      console.log(this.editor.modes);
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
  changeCode: code => dispatch(changeCode(code)),
  changeMode:        mode        => dispatch(changeMode(mode)),
  changeLanguage:    language    => dispatch(changeLanguage(language)),
  changeResultError: error       => dispatch(changeResultError(error)),
  changeResultCode:  (mode,code) => dispatch(changeResultCode(mode,code)),
  changeResultTable: table       => dispatch(changeResultTable(table)),
  changeResultLog:   log         => dispatch(changeResultLog(log)),
  changeStepping:     yesno       => dispatch(changeStepping(yesno))
}};

const mapStateToProps = state => { return {
  mode:      state.mode,
  language:  state.language,
  code:      state.code,
  stepState: state.stepState,
  result:    state.result
}};

export default connect(mapStateToProps,mapDispatchToProps)(Editor);
