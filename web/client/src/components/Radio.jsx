import React, { Component } from 'react';
import './Radio.scss';

class Radio extends Component {
  constructor(props) {
    super(props);
    this.onChange = props.onChange;
    const items = props.items || [{ index: 0, title: 'default' }]
    const disabled = props.disabled || false;
    this.state = {
      index: items[0].index,
      items: items,
    };
  }

  onClick(event, item) {
    if (this.props.disabled) return;
    const index = this.props.selected !== undefined ? this.props.selected : this.state.index;
    if (item.index == index) {
      return;
    }
    this.setState({ index: item.index });
    this.onChange(event, item);
  }

  render() {
    const index = this.props.selected !== undefined ? this.props.selected : this.state.index;
    return (
      <ul className='radio-wrapper'>
        { this.state.items.map(item => {
          const className = (item.index == index ? 'current' : '') + (this.props.disabled ? ' disabled' : '');
          return (<li className={className} key={item.index} onClick={(e) => this.onClick(e,item)}>{item.title}</li>);
        }) }
      </ul>
    );
  }
}

export default Radio;
