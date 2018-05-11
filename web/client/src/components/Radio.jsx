import React, { Component } from 'react';
import './Radio.scss';

class Radio extends Component {
  constructor(props) {
    super(props);
    this.onChange = props.onChange;
    const items = props.items || [{ index: 0, title: 'default' }]
    this.state = {
      index: items[0].index,
      items: items
    };
  }

  onClick(event, item) {
    if (item.index == this.state.index) {
      return;
    }
    this.setState({ index: item.index });
    this.onChange(event, item);
  }

  render() {
    return (
      <ul className='radio-wrapper'>
        { this.state.items.map(item => {
          const className = item.index == this.state.index ? 'current' : '';
          return (<li className={className} key={item.index} onClick={(e) => this.onClick(e,item)}>{item.title}</li>);
        }) }
      </ul>
    );
  }
}

export default Radio;
