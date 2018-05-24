import React, { Component } from 'react';
import { connect } from 'react-redux';
import { Controlled as CodeMirror } from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import '../styles/EditorTheme.scss';
import '../styles/Result.scss';

class Result extends Component {

  constructor(props) {
    super(props);
    // CodeMirror options
    this.options = {
      lineNumbers: true,
      lineWrapping: true,
      readOnly:    true,
      theme:       'editor-theme',
      mode: '[s]rl'
    };
  }

  getStepVariables () {
    // return empty if index is out of bound
    if (this.props.stepState.index > this.props.result.log.table.length) {
      return this.props.result.log.state;
    }
    for (var i=this.props.stepState.index - 1; i >= 0; i--) {
      if ( this.props.result.log.table[i] !== undefined
        && this.props.result.log.table[i].type == 'statement'
        && this.props.result.log.table[i].state.type == 'vartab') {
        return this.props.result.log.table[i].state.table;
      }
    }
    return this.props.result.log.state;
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
    const wrap = cnt => (<div className='result-wrapper notranslate'>{cnt}</div>);
    switch(this.props.mode) {
      case 'run':
        return wrap(
          <div className="variables">
            <ul>
              { this.props.result.table.map(row => {
                  return (<li key={row.id}><span className='variable-id'>{row.id}</span><span className='variable-value'>{row.value}</span></li>);
                })
              }
            </ul>
          </div>
        );
      case 'step':
        const done = this.props.result.log.table.length > 0 && this.props.stepState.index >= this.props.result.log.table.length;
        const failed = this.stepFailed();
        const className = done ? (failed ? 'error' : 'success') : '';
        return wrap(
          <div>
            <div className="variables fixed">
              <ul>
                { this.getStepVariables().map(row => {
                    return (<li key={row.id}><span className='variable-id'>{row.id}</span><span className='variable-value'>{row.value}</span></li>);
                  })
                }
              </ul>
            </div>
            <div className="log">
              <ul className={className}>
                
                { this.props.result.log.table.slice(0,this.props.stepState.index).reverse().map((row,idx) => {
                  if (row.type == 'statement') return (
                    <li key={idx}>{row.statement}</li>
                  );
                  return (<li key={idx}>{row.message}</li>);
                }) }</ul>
            </div>
          </div>
        );
      case 'invert':
      case 'translate':
        return wrap(<CodeMirror
          value={this.props.result[this.props.mode]}
          options={this.options}
        />);
      default: return wrap(<h1><br/><center>This mode is not implemented yet.</center></h1>);
    }

  }
}

const mapStateToProps = state => { return {
  mode:      state.mode,
  language:  state.language,
  result:    state.result,
  stepState: state.stepState
}};

export default connect(mapStateToProps)(Result);
