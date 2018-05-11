import React, { Component } from 'react';
import { connect } from 'react-redux';
import Radio from './Radio';
import Button from './Button';
import { changeMode, changeLanguage, changeResultCode, changeResultError, changeResultTable } from '../actions/index';
import './Header.scss';
import * as api from '../api';

import playIcon from '../images/icons/play.svg';
import saveIcon from '../images/icons/save.svg';
import openIcon from '../images/icons/open.svg';
import tempIcon from '../images/icons/template.svg';

const items = [
  {
    index: 'srl',
    title: 'SRL'
  },
  {
    index: 'rl',
    title: 'RL'
  }
];
const modeItems = [
  {
    index: 'run',
    title: 'Run'
  },
  {
    index: 'step',
    title: 'Step'
  },
  {
    index: 'invert',
    title: 'Invert'
  },
  {
    index: 'translate',
    title: 'Translate'
  }
];

class Header extends Component {

  changeMode(event,item) {
    this.props.changeMode(item.index);
  }

  changeLanguage(event,item) {
    this.props.changeLanguage(item.index);
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

  getActions(mode) {
    switch (mode) {
      case 'run':
        return (<Button onClick={this.runProgram.bind(this)}><img src={playIcon}/></Button>);
      case 'invert':
        return (<Button onClick={this.invertProgram.bind(this)}><img src={playIcon}/></Button>);
      case 'translate':
        return (<Button onClick={this.translateProgram.bind(this)}><img src={playIcon}/></Button>);
      default: return;
    }
  }

  render() {
    return (
      <div className='header-wrapper'>
        <span className='controls'>
          <Radio items={items} onChange={this.changeLanguage.bind(this)} />
          <span className='actions'>
            <Button><img src={tempIcon}/></Button>
            <Button><img src={openIcon}/></Button>
            <Button><img src={saveIcon}/></Button>
          </span>
        </span>
        <span className='mode-controls'>
          <span className='actions'>
            { this.getActions(this.props.mode) }
          </span>
          <span className='modes'>
            <Radio items={modeItems} onChange={this.changeMode.bind(this)} />
          </span>
        </span>
      </div>
    );
  }
}

const mapStateToProps = state => { return {
  mode:     state.mode,
  language: state.language,
  code:     state.code
}};

const mapDispatchToProps = dispatch => { return {
  changeMode:        mode        => dispatch(changeMode(mode)),
  changeLanguage:    language    => dispatch(changeLanguage(language)),
  changeResultError: error       => dispatch(changeResultError(error)),
  changeResultCode:  (mode,code) => dispatch(changeResultCode(mode,code)),
  changeResultTable: table       => dispatch(changeResultTable(table))
}};

export default connect(mapStateToProps, mapDispatchToProps)(Header);
