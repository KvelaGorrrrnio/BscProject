import React, { Component } from 'react';
import { connect } from 'react-redux';
import { Controlled as CodeMirror } from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import './default-bsc.scss';
import './Result.scss';

class Result extends Component {

  constructor(props) {
    super(props);
    // CodeMirror options
    this.options = {
      lineNumbers: true,
      readOnly:    true,
      theme:       'default-bsc'
    };
  }

  render() {
    const wrap = cnt => (<div className='result-wrapper'>{cnt}</div>);
    switch(this.props.mode) {
      case 'run':
        return wrap(
          <ul>{ this.props.result.table.map(row => {
            return (<li key={row.id}>{row.id} : {row.value}</li>);
          }) }</ul>
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
  mode:     state.mode,
  language: state.language,
  result:   state.result

}};

export default connect(mapStateToProps)(Result);
