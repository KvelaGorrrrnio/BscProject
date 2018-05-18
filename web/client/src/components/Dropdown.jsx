import React, { Component } from 'react';
import './Dropdown.scss'

class Dropdown extends Component {

  constructor(props) {
    super(props);
  }

  renderItem(item,idx) {
    if (item.type == 'category') {
      return (<li key={idx}>{ item.title }
        <ul className='dropdown-list'>
          { item.children.map(this.renderItem.bind(this)) }
        </ul>
      </li>);
    } else {
      return (
        <li key={idx} onClick={item.onClick}>{ item.title }</li>
      );
    }
  }

  render() {
    const disabled = this.props.disabled ? ' disabled' : '';
    return (
      <div className={'dropdown' + disabled}>
        {this.props.children}
        <ul>
          { this.props.items.map(this.renderItem.bind(this)) }
        </ul>
      </div>
    );
  }
}

export default Dropdown;
