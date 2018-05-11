import React, { Component } from 'react';
import Header from './Header';
import Editor from './Editor';
import Result from './Result';
import './App.scss';

class App extends Component {
  render() {
    return (
      <div className='app-wrapper'>
        <Header/>
        <Editor/>
        <Result/>
      </div>
    );
  }
}

export default App
