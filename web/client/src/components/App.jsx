import React, { Component } from 'react';
import Header from './Header';
import Editor from './Editor';
import Result from './Result';
import SaveModal from './SaveModal';
import './App.scss';

class App extends Component {
  render() {
    return (
      <div className='app-wrapper'>
        <Header/>
        <Editor/>
        <Result/>
        <SaveModal/>
      </div>
    );
  }
}

export default App
