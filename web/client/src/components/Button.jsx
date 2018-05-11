import React, { Component } from 'react';
import './Button.scss'

class Button extends Component {

  handleClick(event) {
    if (!this.props.onClick) return;
    this.props.onClick(event);
  }

  render() {
    return (
      <div className='button' onClick={this.handleClick.bind(this)}>{this.props.children}</div>
    );
  }
}

export default Button;
