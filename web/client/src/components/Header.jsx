import React, { Component } from 'react';
import { connect } from 'react-redux';
import Radio from './Radio';
import Button from './Button';
import Dropdown from './Dropdown';
import * as actions from '../actions/index';
import * as api from '../api';
import './Header.scss';

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

  constructor() {
    super()
    this.state = { templates: {
      srl: [], rl: [],
    } };
  }

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

  selectTemplate(lng,template) {
    const changeLanguage = this.props.changeLanguage;
    api.template(template,(err,cnt) => {
      if (err) return;
      changeLanguage(lng);
      this.props.changeCode(cnt.code);
    });
  }

  componentDidMount() {
    api.templates((err,templates) => {
      if (err) return;
      this.setState({
        templates: {
          srl: {
            type: 'category',
            title: 'SRL',
            children: templates.srl.map(template => { return {
              type: 'item',
              title: template,
              onClick: () => { this.selectTemplate.bind(this)('srl',template) }
            }})
          },
          rl: {
            type: 'category',
            title: 'RL',
            children: templates.rl.map(template => { return {
              type: 'item',
              title: template,
              onClick: () => { this.selectTemplate.bind(this)('rl',template) }
            }})
          },
        }
      });
    });
  }

  stopStepping() {
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

  saveLocal() {
    this.props.showSaveModal();
  }

  openLocal() {
    this.props.showOpenModal();
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

  getModeName(mode) {
    for (const i in modeItems) {
      const m = modeItems[i];
      console.log(m);
      if(m.index==mode) return m.title;
    }
    console.log("NADSNÆDS")
    return mode;
  }

  render() {
    return (
      <div className='header-wrapper'>
        <span className='controls'>
          <Radio items={items} selected={this.props.language} onChange={this.changeLanguage.bind(this)} disabled={this.props.stepState.stepping} />
          <span className='actions'>
            <Dropdown disabled={this.props.stepState.stepping} items={[this.state.templates.srl,this.state.templates.rl]}><img src={tempIcon}/></Dropdown>
            <Button disabled={this.props.stepState.stepping} onClick={this.openLocal.bind(this)}><img src={openIcon}/></Button>
            <Button disabled={this.props.stepState.stepping} onClick={this.saveLocal.bind(this)}><img src={saveIcon}/></Button>
          </span>
        </span>
        <span className='mode-controls'>
          <span className='actions'>
            { this.getActions(this.props.mode) }
          </span>
          <span className='modes'>
            <div className='selected-mode'>
              <span>{this.getModeName(this.props.mode)}</span>
              <Radio items={modeItems} onChange={this.changeMode.bind(this)} disabled={this.props.stepState.stepping} />
            </div>
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
  changeMode:        mode        => dispatch(actions.changeMode(mode)),
  changeCode:        code        => dispatch(actions.changeCode(code)),
  changeLanguage:    language    => dispatch(actions.changeLanguage(language)),
  changeResultError: error       => dispatch(actions.changeResultError(error)),
  changeResultCode:  (mode,code) => dispatch(actions.changeResultCode(mode,code)),
  changeResultTable: table       => dispatch(actions.changeResultTable(table)),
  changeResultLog:   log         => dispatch(actions.changeResultLog(log)),
  startStepping:     ()          => dispatch(actions.startStepping()),
  stopStepping:      ()          => dispatch(actions.stopStepping()),
  nextStep:          ()          => dispatch(actions.nextStep()),
  prevStep:          ()          => dispatch(actions.prevStep()),
  showSaveModal:     ()          => dispatch(actions.showSaveModal()),
  showOpenModal:     ()          => dispatch(actions.showOpenModal()),
}};

export default connect(mapStateToProps, mapDispatchToProps)(Header);
