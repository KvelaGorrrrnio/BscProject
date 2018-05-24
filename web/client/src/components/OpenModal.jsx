import React, { Component } from 'react';
import { connect } from 'react-redux';
import { changeCode, hideOpenModal } from '../actions/index';
import Button from './Button';
import DropZone from './DropZone';
import '../styles/Modal.scss';

class OpenModal extends Component {

  constructor() {
    super();
    this.state = {
      index: -1
    }
  }

  getFiles() {
    if (localStorage.files === undefined) {
      localStorage.files = JSON.stringify([]);
    }
    return JSON.parse(localStorage.files);
  }

  fileExists(name) {
    const filename = name + '.' +  this.props.language;
    return this.getFiles().filter(file => { return filename == file.title }).length == 1;
  }

  openFile() {
    const files = this.getFiles();
    if (this.state.index < 0 || this.state.index >= files.length) return;
    const file = files[this.state.index];
    if (!file) return;
    this.props.changeCode(file.code);
    this.setState({ index: -1 });
    this.refs.fileImporter.value = '';
    this.props.hideOpenModal();
  }

  trimExt(name) {
    return name.substr(0,name.lastIndexOf('.'));
  }

  hide() {
    this.props.hideOpenModal();
  }

  openFileDialog() {
    this.refs.fileImporter.click();
  }

  handleFileDrop(event,files) {
    if (files.length == 0) return;
    this.importFile(files[0]);
  }
  handleFileSelect(event) {
    if (event.target.files.length == 0) return;
    const file = event.target.files[0];
    this.importFile(file);
  }

  importFile(file) {
    if ( ['srl', 'rl'].includes(this.getExt(file.name)) ) {
      const reader = new FileReader;
      reader.onload = () => {
        this.props.changeCode(reader.result);
        this.setState({ index: -1 });
        this.refs.fileImporter.value = '';
        this.props.hideOpenModal();
      };
      reader.readAsText(file)
    } else {
      alert('\'' + file.name + '\' should have either the .srl or .rl as file extension.');
    }
  }

  getExt(filename) {
  return filename.slice((filename.lastIndexOf(".") - 1 >>> 0) + 2);
}

  supportsImport() {
    return window.File && window.FileReader && window.FileList && window.Blob;
  }

  render () {
    const importStyle = this.supportsImport() ? { display: 'none' } : {};
    return (
      <div className={'modal-wrapper ' + (this.props.modal.open ? '' : 'hidden ') + (this.props.className||'')}>
        <div className='modal-shade'></div>
        <div className='modal-content'>
          <h2>Open or import your script</h2>
          <div className='_2column'>
            <DropZone onChange={this.handleFileDrop.bind(this)} onClick={this.openFileDialog.bind(this)}>Drag and drop files here to import.<br/><br/>Or click for file dialog.</DropZone>
              <input type='file' style={{ display: 'none' }} onChange={this.handleFileSelect.bind(this)} ref='fileImporter' />
            <p className='fancy'><span> or </span></p>
            <Button className='half' onClick={this.openFile.bind(this)}>Open</Button>
            <Button className='half' onClick={this.hide.bind(this)}>Cancel</Button>
          </div><div className='_2column'>
            <ul className='notranslate'>
              { this.getFiles().map((file,idx) => { return (
                <li key={idx} className={this.state.index==idx ? 'selected' : ''} onClick={() => {this.setState({ index: idx }) }}>
                  {file.title}
                </li>
              ); }) }
            </ul>
          </div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => { return {
  code:      state.code,
  language:  state.language,
  modal: {
    open: state.modal.open
  }
}};

const mapDispatchToProps = dispatch => { return {
  changeCode:     code => dispatch(changeCode(code)),
  hideOpenModal: ()   => dispatch(hideOpenModal()),
}};

export default connect(mapStateToProps,mapDispatchToProps)(OpenModal);
