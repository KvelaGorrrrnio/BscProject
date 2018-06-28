import React, { Component } from 'react';
import { connect } from 'react-redux';
import { changeCode, changeTheme } from '../actions/index';
import Header from './Header';
import Editor from './Editor';
import Result from './Result';
import SaveModal from './SaveModal';
import OpenModal from './OpenModal';
import '../styles/App.scss';

function isASCII(str) {
    return /^[\x00-\x7F]*$/.test(str);
}

class App extends Component {

  componentDidMount() {
    // Check if has share-link content
    const url = new URL(location.href);
    try {
      const code = atob(url.searchParams.get("code"));
      if (isASCII(code)) {
        this.props.changeCode(code);
      }
    }
    catch (err) {}
    // Get last used theme
    if (localStorage.theme !== undefined) {
      this.props.changeTheme(localStorage.theme);
    }
  }

  render() {
    return (
      <div className={'app-wrapper theme-'+this.props.theme}>
        <Header/>
        <Editor/>
        <Result/>
        <SaveModal/>
        <OpenModal/>
      </div>
    );
  }
}

const mapStateToProps = state => { return {
  theme: state.theme
}};

const mapDispatchToProps = dispatch => { return {
  changeCode:  code  => dispatch(changeCode(code)),
  changeTheme: theme => dispatch(changeTheme(theme))
}};

export default connect(mapStateToProps,mapDispatchToProps)(App);
