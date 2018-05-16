import React, { Component } from 'react';
import { connect } from 'react-redux';
import Radio from './Radio';
import Button from './Button';
import { changeMode, changeLanguage, changeResultCode, changeResultError, changeResultTable, changeResultLog, startStepping, stopStepping, nextStep, prevStep } from '../actions/index';
import './Header.scss';
import * as api from '../api';

import playIcon from '../images/icons/play.svg';
import stepIcon from '../images/icons/step.svg';
import stepAllIcon from '../images/icons/stepall.svg';
import stepPrevIcon from '../images/icons/stepprev.svg';
import stopIcon from '../images/icons/stop.svg';
import resetIcon from '../images/icons/reset.svg';
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

  runProgram(event, log=false) {
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

  stopStepping() {
    console.log(this.props);
    this.props.changeResultLog({ state: { table: [] }, table: [] });
    this.props.stopStepping();
  }

  resetStepping() {
    this.props.startStepping();
  }

  nextStep() {
    // Check if has executed last step
    if (this.props.stepState.index >= this.props.result.log.table.length) {
      return;
    }
    this.props.nextStep();
  }
  prevStep() {
    // Check if has executed last step
    if (this.props.stepState.index <= 0) {
      return;
    }
    this.props.prevStep();
  }

  stepAll() {
    var i = this.props.stepState.index;
    const n = this.props.result.log.table.length;
    while (i < n) { this.nextStep(); i++ }
  }

  getActions(mode) {
    switch (mode) {
      case 'run':
        return (<Button onClick={this.runProgram.bind(this)} disabled={this.props.stepState.stepping}><img src={playIcon}/></Button>);
      case 'step':
        if (this.props.stepState.stepping) {
          const lastStep  = this.props.stepState.index >= this.props.result.log.table.length;
          const firstStep = this.props.stepState.index == 0;
          return (
            <div>
              <Button disabled={firstStep} onClick={this.prevStep.bind(this)}><img src={stepPrevIcon} /></Button>
              <Button disabled={lastStep}  onClick={this.nextStep.bind(this)}><img src={stepIcon} /></Button>
              <Button disabled={firstStep} onClick={this.props.startStepping}><img src={resetIcon} style={{ height: '18px', padding: '1px' }}/></Button>
              <Button onClick={this.stopStepping.bind(this)}><img src={stopIcon} style={{ height: '18px', padding: '1px' }}/></Button>
              <Button disabled={lastStep} onClick={this.stepAll.bind(this)}><img src={stepAllIcon} style={{ height: '18px', padding: '1px' }}/></Button>
            </div>);
        } else return (
          <Button onClick={(e) => { this.runProgram.bind(this)(e,true) }}><img src={playIcon}/></Button>
        );
      case 'invert':
        return (<Button onClick={this.invertProgram.bind(this)} disabled={this.props.stepState.stepping}><img src={playIcon}/></Button>);
      case 'translate':
        return (<Button onClick={this.translateProgram.bind(this)} disabled={this.props.stepState.stepping}><img src={playIcon}/></Button>);
      default: return;
    }
  }

  render() {
    return (
      <div className='header-wrapper'>
        <span className='controls'>
          <Radio items={items} onChange={this.changeLanguage.bind(this)} disabled={this.props.stepState.stepping} />
          <span className='actions'>
            <Button disabled={this.props.stepState.stepping}><img src={tempIcon}/></Button>
            <Button disabled={this.props.stepState.stepping}><img src={openIcon}/></Button>
            <Button disabled={this.props.stepState.stepping}><img src={saveIcon}/></Button>
          </span>
        </span>
        <span className='mode-controls'>
          <span className='actions'>
            { this.getActions(this.props.mode) }
          </span>
          <span className='modes'>
            <Radio items={modeItems} onChange={this.changeMode.bind(this)} disabled={this.props.stepState.stepping} />
          </span>
        </span>
      </div>
    );
  }
}

const mapStateToProps = state => { return {
  mode:     state.mode,
  language: state.language,
  code:     state.code,
  stepState: state.stepState,
  result: { log: state.result.log },
}};

const mapDispatchToProps = dispatch => { return {
  changeMode:        mode        => dispatch(changeMode(mode)),
  changeLanguage:    language    => dispatch(changeLanguage(language)),
  changeResultError: error       => dispatch(changeResultError(error)),
  changeResultCode:  (mode,code) => dispatch(changeResultCode(mode,code)),
  changeResultTable: table       => dispatch(changeResultTable(table)),
  changeResultLog:   log         => dispatch(changeResultLog(log)),
  startStepping:     ()          => dispatch(startStepping()),
  stopStepping:      ()          => dispatch(stopStepping()),
  nextStep:          ()          => dispatch(nextStep()),
  prevStep:          ()          => dispatch(prevStep()),
}};

export default connect(mapStateToProps, mapDispatchToProps)(Header);
