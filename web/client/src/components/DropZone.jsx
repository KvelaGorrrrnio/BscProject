import React, { Component } from 'react';
import './DropZone.scss';

class DropZone extends Component {

  handleDragOver(event) {
    event.stopPropagation();
    event.preventDefault();
    event.dataTransfer.dropEffect = 'copy';
  }

  handleDrop(event) {
    event.stopPropagation();
    event.preventDefault();
    if (this.props.onChange !== undefined) {
      this.props.onChange(event,event.dataTransfer.files);
    }
  }

  handleClick(event) {
    if (this.props.onClick !== undefined) {
      this.props.onClick(event);
    }
  }

  render() {
    return (
      <div className='dropzone' onClick={this.handleClick.bind(this)} onDrop={this.handleDrop.bind(this)} onDragOver={this.handleDragOver.bind(this)}><div>{this.props.children}</div></div>
    );
  }

}

export default DropZone;
