import React, { Component } from 'react';
import { connect } from 'react-redux';
import { changeCode, hideSaveModal } from '../actions/index';
import Button from './Button';
import { download } from '../download';
import '../styles/Modal.scss';
import copyIcon from '../images/icons/copy.svg';

class SaveModal extends Component {

  constructor() {
    super();
    this.state = {
      filename: 'myFirstScript',
      sharelink: ''
    };
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

  saveFile() {
    const filename = this.state.filename + '.' + this.props.language;
    const code = this.props.code;
    if (this.fileExists(this.state.filename)) {
      const newFiles = this.getFiles().map(file => {
        if (file.title == filename) file.code = code;
        return file
      });
      localStorage.files = JSON.stringify(newFiles);
    } else {
      const newFiles = this.getFiles()
      newFiles.push({
        title: filename,
        code
      });
      localStorage.files = JSON.stringify(newFiles);
    }
    this.props.hideSaveModal();
  }

  onChange(event) {
    if (event.target.value.indexOf('.') < 0 && event.target.value != '') return;
    var filename = this.trimExt(event.target.value);
    const extPos = event.target.value.lastIndexOf('.'+this.props.language);
    if (event.target.value.length > extPos + 1 + this.props.language.length && extPos >= 0) {
      filename += event.target.value.substr(extPos + 1 + this.props.language.length);
    }
    this.setState({filename: filename});
  }
  trimExt(name) {
    return name.substr(0,name.lastIndexOf('.'));
  }

  hide() {
    this.setState({ sharelink: '' });
    this.props.hideSaveModal();
  }

  exportScript() {
    const filename = (this.state.filename.trim() == '' ? 'somescript' : this.state.filename) + '.' + this.props.language;
    download('data:text/plain,'+encodeURIComponent(this.props.code), filename, "text/plain");
  }

  getSharelink() {
    const url = [location.protocol, '//', location.host, location.pathname].join('');
    this.setState({ sharelink: url + '?code=' + btoa(this.props.code) })
  }
  
  selectSharelink() {
    const sharelink = this.refs.sharelink;
    sharelink.setSelectionRange(0,sharelink.value.length);
  }

  copySharelink() {
    this.selectSharelink();
    document.execCommand('copy');
    this.refs.sharelinkcopy.classList.add('success');
    setTimeout(() => { this.refs.sharelinkcopy.classList.remove('success'); }, 700);
  }

  render () {
    const saveText = this.fileExists(this.state.filename) ? 'Overwrite' : 'Save';
    const filename = this.state.filename + '.' + this.props.language;
    return (
      <div className={'modal-wrapper ' + (this.props.modal.save ? '' : 'hidden ') + (this.props.className||'')}>
        <div className='modal-shade'></div>
        <div className='modal-content'>
          <h2>Save, export or share your script</h2>
          <div className='_2column'>
            <input type='text' value={filename} onChange={this.onChange.bind(this)} />
            <Button className='half' disabled={this.state.filename==''} onClick={this.saveFile.bind(this)}>{saveText}</Button>
            <Button className='half' onClick={this.hide.bind(this)}>Cancel</Button>
            <p className='fancy'><span> or </span></p>
            <Button onClick={this.exportScript.bind(this)}>Export to computer</Button>
            <Button onClick={this.getSharelink.bind(this)}>Share with a link</Button>
            <div className="sharelink">
              <input type='text' ref='sharelink' readOnly onClick={this.selectSharelink.bind(this)} value={this.state.sharelink} />
              <span ref="sharelinkcopy" onClick={this.copySharelink.bind(this)}><img src={copyIcon} className={'icon' + (this.state.sharelink=='' ? ' disabled' : '')} /></span>
            </div>
          </div><div className='_2column'>
            <ul className='notranslate'>
              { this.getFiles().map((file,idx) => { return (
                <li key={idx} className={filename == file.title ? 'selected' : ''} onClick={() => {this.setState({ filename: this.trimExt(file.title) }) }}>
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
    save: state.modal.save
  }
}};

const mapDispatchToProps = dispatch => { return {
  changeCode:     code => dispatch(changeCode(code)),
  hideSaveModal: ()   => dispatch(hideSaveModal()),
}};

export default connect(mapStateToProps,mapDispatchToProps)(SaveModal);
