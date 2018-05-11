import React, { Component } from 'react';
import { connect } from 'react-redux';
import { changeMode, changeCode, changeLanguage, changeResultCode, changeResultError, changeResultTable } from '../actions/index';
import * as api from '../api';
import { Controlled as CodeMirror } from 'react-codemirror2';
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
      theme: 'default-bsc',
      extraKeys: {
        'Cmd-Enter' : cm => {
          switch (this.props.mode) {
            case 'run': this.runProgram(); break;
            case 'step': break;
            case 'invert': this.invertProgram(); break;
            case 'translate': this.translateProgram(); break;
          }
        },
        'Ctrl-Enter' : cm => {
          switch (this.props.mode) {
            case 'run': this.runProgram(); break;
            case 'step': break;
            case 'invert': this.invertProgram(); break;
            case 'translate': this.translateProgram(); break;
          }
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

  runProgram() {
    api.run(this.props.language,{
      code: this.props.code
    }, (err,result) => {
      if (err) this.props.changeResultError(err);
      else     this.props.changeResultTable(result.table);
    });
  }


  render() {
    const err = this.props.result.error;
    if (this.editor) {
      // Reset
      console.log("Editor has lines: "+this.editor.doc.size);
      for (var i=0; i<this.editor.doc.size; i++) {
        this.editor.doc.removeLineClass(i, 'wrap', 'error');
      }
      if ('position' in err) {
        const line = parseInt(err.position.line)-1;
        console.log("lines",line);
        this.editor.doc.addLineClass(line, 'wrap', 'error');
      }
    }
    const className = err.type ? 'editor-wrapper has-error' : 'editor-wrapper';
    const Error = err.type ? (<div className='error-wrapper'>{err.message}</div>) : null;
    return (
      <div className={className}>
        <CodeMirror
          editorDidMount={this.setEditor.bind(this)}
          value={this.props.code}
          options={this.options}
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
  changeResultTable: table       => dispatch(changeResultTable(table))
}};

const mapStateToProps = state => { return {
  mode:     state.mode,
  language: state.language,
  code:     state.code,
  result:   {
    error: state.result.error
  }
}};

export default connect(mapStateToProps,mapDispatchToProps)(Editor);
