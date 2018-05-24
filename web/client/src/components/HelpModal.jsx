import React, { Component } from 'react';
import { connect } from 'react-redux';
import { hideHelpModal } from '../actions/index'
import Button from './Button';
import '../styles/HelpModal.scss';

import overviewContent from '../help/overview';
import rlContent from '../help/rl';
import srlContent from '../help/srl';

import runMode from '../help/run';
import stepMode from '../help/step';
import invertMode from '../help/invert';
import translateMode from '../help/translate';

const sections =
  [ { title: '', articles:
    [ { title: 'Overview', content: overviewContent }
    ] }
  , { title: 'Languages', articles:
    [ { title: 'RL', content: rlContent }
    , { title: 'SRL', content: srlContent }
    ] }
  , { title: 'Modes', articles:
    [ { title: 'Run', content: runMode }
    , { title: 'Step', content: stepMode }
    , { title: 'Invert', content: invertMode }
    , { title: 'Translate', content: translateMode }
    ] }
  ];

class HelpModal extends Component {

  constructor() {
    super();
    this.state = {
      section: 0,
      article: 0
    };
  }

  choose(section, article) {
    this.setState({
      section,
      article
    });
  }

  getContent(section,article) {
    for (var i=0; i<sections.length; i++) {
      for (var j=0; j<sections[i].articles.length; j++) {
        if (i==section && j==article) {
          return sections[i].articles[j].content;
        }
      }
    }
    return '';
  }

  render() {
    return (
      <div className={'modal-wrapper help-modal' + (this.props.modal.help ? '' : ' hidden')}>
        <div className='modal-shade'></div>
        <div className='modal-content'>
          <div className='sections'>
            { sections.map((section,sidx) => {
              const title = section.title!='' ? <h3>{section.title}</h3> : '';
              return (
                <div key={sidx}>
                  { title }
                  <ul>
                  { section.articles.map((article,aidx) => {
                    return (<li className={sidx==this.state.section && aidx==this.state.article ? 'selected' : ''} key={aidx} onClick={() => {this.choose(sidx,aidx)}}>{article.title}</li>)
                  }) }
                  </ul>
                </div>
              );
            }) }
          <Button className='close' onClick={this.props.hideHelpModal.bind(this)}>Close</Button>
          </div>
          <div className='article'>
          {this.getContent(this.state.section,this.state.article)}
          </div>
        </div>
      </div>
    );
  }

}

const mapStateToProps = state => { return {
  modal: {
    help: state.modal.help
  }
}};

const mapDispatchToProps = dispatch => { return {
  hideHelpModal: ()   => dispatch(hideHelpModal()),
}};

export default connect(mapStateToProps,mapDispatchToProps)(HelpModal);
