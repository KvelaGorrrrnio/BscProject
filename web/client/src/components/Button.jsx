import React, { Component } from 'react';
import './Button.scss'

class Button extends Component {

  constructor(props) {
    super(props)
  }

  handleClick(event) {
    if (!this.props.onClick || this.props.disabled) return;
    this.props.onClick(event);
  }

  render() {
    const disabled = this.props.disabled ? ' disabled' : '';
    return (
      <div className={'button' + disabled} onClick={this.handleClick.bind(this)}>{this.props.children}</div>
    );
  }
}

export default Button;
