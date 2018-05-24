import React, { Component } from 'react';
import { connect } from 'react-redux';
import { changeCode } from '../actions/index';
import Header from './Header';
import Editor from './Editor';
import Result from './Result';
import SaveModal from './SaveModal';
import OpenModal from './OpenModal';
import HelpModal from './HelpModal';
import '../styles/App.scss';

function isASCII(str) {
    return /^[\x00-\x7F]*$/.test(str);
}

class App extends Component {

  componentDidMount() {
    const url = new URL(location.href);
    try {
      const code = atob(url.searchParams.get("code"));
      if (isASCII(code)) {
        this.props.changeCode(code);
      }
    }
    catch (err) {}
  }

  render() {
    return (
      <div className='app-wrapper'>
        <Header/>
        <Editor/>
        <Result/>
        <SaveModal/>
        <OpenModal/>
        <HelpModal/>
      </div>
    );
  }
}

const mapDispatchToProps = dispatch => { return {
  changeCode: code => dispatch(changeCode(code))
}};

export default connect(null,mapDispatchToProps)(App);
